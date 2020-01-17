with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events;
with Logging_Support;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

with Epoch_Support; use Epoch_Support;

with XAda.Dispatching.TTS;
with TT_Utilities;
with TT_Patterns;
with TT_Mixed_Criticality; use TT_Mixed_Criticality;

package body TTS_Example_D is

   Number_Of_Work_Ids : constant := 5;
   Number_Of_Sync_Ids : constant := 1;

   package TTS is new XAda.Dispatching.TTS
     (TT_Mixed_Criticality.Dual_Criticality_Levels, Number_Of_Work_Ids, Number_Of_Sync_Ids);

   package TT_Util is new TT_Utilities (TTS);
   use TT_Util;

   package TT_Patt is new TT_Patterns (TTS);
   use TT_Patt;

   --  Auxiliary for printing times --
   function Now (Current : Time) return String is
     (Duration'Image ( To_Duration (Current - TTS.Get_First_Plan_Release) * 1000) & " ms " &
      "|" & Duration'Image ( To_Duration (Current - TTS.Get_Last_Plan_Release) * 1000) & " ms ");

   procedure TT_Put_Line (Item : in  String) is
      Work_Slot : TTS.Any_Work_Slot := TTS.Any_Work_Slot(TTS.Get_Current_Slot);
   begin
      Put_Line ("[" & Work_Slot.Work_Id'Image & "] @ " & Work_Slot.Criticality_Level'Image & " > "
                & Item);
   end TT_Put_Line;

   -- TT tasks --

   type First_Init_Task is new Simple_Task_State
     with
      record
         Iter    : Natural := 0;
      end record;
   procedure Initialize (S : in out First_Init_Task) is null;
   procedure Main_Code (S : in out First_Init_Task);

   Wk1_Code : aliased First_Init_Task;
   Wk1 : Simple_TT_Task
     (Work_Id => 3,
      Task_State => Wk1_Code'Access,
      Synced_Init => False);

   type First_IMF_Task is new Initial_Mandatory_Final_Task_State
     with
      record
         Counter : Natural := 0;
         Iter    : Natural := 0;
      end record;
   procedure Initialize (S : in out First_IMF_Task) is null;
   procedure Initial_Code (S : in out First_IMF_Task);
   procedure Mandatory_Code (S : in out First_IMF_Task);
   procedure Final_Code (S : in out First_IMF_Task);

   Wk2_State : aliased First_IMF_Task;
   Wk2 : InitialMandatorySliced_Final_TT_Task
     (Work_Id => 1,
      Task_State => Wk2_State'Access,
      Synced_Init => False);

   type Second_Init_Task is new Simple_Task_State with null record;
   procedure Initialize (S : in out Second_Init_Task) is null;
   procedure Main_Code (S : in out Second_Init_Task);

   Wk3_Code : aliased Second_Init_Task;
   Wk3 : Simple_TT_Task
     (Work_Id => 4,
      Task_State => Wk3_Code'Access,
      Synced_Init => False);

   type Second_IMF_Task is new Initial_Mandatory_Final_Task_State with
      record
         Counter : Natural := 0;
      end record;
   procedure Initialize (S : in out Second_IMF_Task) is null;
   procedure Initial_Code (S : in out Second_IMF_Task);
   procedure Mandatory_Code (S : in out Second_IMF_Task);
   procedure Final_Code (S : in out Second_IMF_Task);

   Wk4_Code : aliased Second_IMF_Task;
   Wk4 : InitialMandatorySliced_Final_TT_Task
     (Work_Id => 2,
      Task_State => Wk4_Code'Access,
      Synced_Init => False);

   type Synced_ET_Task is new Initial_OptionalFinal_Task_State with
      record
         Counter : Natural := 0;
      end record;
   procedure Initialize (S : in out Synced_ET_Task) is null;
   procedure Initial_Code (S : in out Synced_ET_Task);
   function Final_Is_Required (S : in out Synced_ET_Task) return Boolean;
   procedure Final_Code (S : in out Synced_ET_Task);

   Wk6_Code : aliased Synced_ET_Task;
   Wk6 : SyncedInitial_OptionalFinal_ET_Task
     (Sync_Id => 1,
      Work_Id => 5,
      Task_State => Wk6_Code'Access,
      Synced_Init => False);

   --  Actions of sequence initialisations
   procedure Main_Code (S : in out First_Init_Task) is  --  Simple_TT task with ID = 1
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      TT_Put_Line ("First_Init_Task.Main_Code ended at " & Now (Clock));

      S.Iter := S.Iter + 1;
      if S.Iter mod 4 = 0 then
         TTS.Set_System_Criticality_Level(LO);
      end if;

   end Main_Code;

   procedure Main_Code (S : in out Second_Init_Task) is  --  Simple_TT task with ID = 3
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      TT_Put_Line ("Second_Init_Task.Main_Code ended at " & Now (Clock));
   end Main_Code;

   --  Actions of first sequence: IMs-F task with ID = 2
   procedure Initial_Code (S : in out First_IMF_Task) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      New_Line;
      Put_Line ("------------------------");
      Put_Line ("Starting plan @ CL " & TTS.Get_System_Criticality_Level'Image);
      Put_Line ("------------------------");
      New_Line;

      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                 Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      S.Counter := 0;
      S.Iter := S.Iter + 1;

      TT_Put_Line ("First_IMF_Task.Initial_Code ended at " & Now (Clock));
   end Initial_Code;

   procedure Mandatory_Code (S : in out First_IMF_Task) is
   begin
      TT_Put_Line ("First_IMF_Task.Mandatory_Code sliced started at " & Now (Clock));

      while S.Counter < 200_000 + (100_000 * (S.Iter mod 2)) loop
         S.Counter := S.Counter + 1;
         if S.Counter mod 20_000 = 0 then
            TT_Put_Line ("First_IMF_Task.Mandatory_Code sliced step " & Now (Clock));
         end if;
      end loop;

      TT_Put_Line ("First_IMF_Task.Mandatory_Code sliced ended at " & Now (Clock));
   end Mandatory_Code;

   procedure Final_Code (S : in out First_IMF_Task) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      TT_Put_Line ("First_IMF_Task.Final_Code Seq. 1 with Counter =" & S.Counter'Image & " at" & Now (Clock));
   end Final_Code;

   --  Actions of Second sequence: IMs-F task with ID = 4
   procedure Initial_Code (S : in out Second_IMF_Task) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      S.Counter := 0;
      TT_Put_Line ("Second_IMF_Task.Initial_Code ended at " & Now (Clock));
  end Initial_Code;

   procedure Mandatory_Code (S : in out Second_IMF_Task) is
   begin
      TT_Put_Line ("Second_IMF_Task.Mandatory_Code sliced started at " & Now (Clock));
      while S.Counter < 200_000 loop
         S.Counter := S.Counter + 1;
         if S.Counter mod 20_000 = 0 then
            TT_Put_Line ("Second_IMF_Task.Mandatory_Code sliced step " & Now (Clock));
         end if;
      end loop;
      TT_Put_Line ("Second_IMF_Task.Mandatory_Code sliced ended at " & Now (Clock));
   end Mandatory_Code;

   procedure Final_Code (S : in out Second_IMF_Task) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      TT_Put_Line ("Second_IMF_Task.Final_Code Seq. 2 with Counter =" & S.Counter'Image  & " at" & Now (Clock));
   end Final_Code;

   procedure Initial_Code (S : in out Synced_ET_Task) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      Put_line( "Synced" & Integer (S.Sync_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      S.Counter := S.Counter + 1;
      Put_Line ("Synced_ET_Task.Synced_Code with counter = " & S.Counter'Image  & " at" & Now (Clock));
   end Initial_Code;

   function Final_Is_Required (S : in out Synced_ET_Task) return Boolean is
      Condition : Boolean;
   begin
      Condition := (S.Counter mod 2 = 1);
      Put_Line ("Synced_ET_Task.Final_Is_Required with condition = " & Condition'Image  & " at" & Now (Clock));
      return Condition;
   end Final_Is_Required;

   procedure Final_Code (S : in out Synced_ET_Task) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      TT_Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      TT_Put_Line ("Synced_ET_Task.Final_Code with counter = " & S.Counter'Image  & " at" & Now (Clock));
   end Final_Code;

   protected Criticality_Manager
     with Priority => System.Interrupt_Priority'Last is

      procedure Overrun_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

   end Criticality_Manager;

   protected body Criticality_Manager is

      procedure Overrun_Handler
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event) is
      begin
         Put_Line("Overrun detected!!");
         TTS.Set_System_Criticality_Level(HI);
      end Overrun_Handler;

   end Criticality_Manager;

   ms : constant Time_Span := Milliseconds (1);
   zero : constant Time_Span := Time_Span_Zero;

   --  The TT plan
   TT_Plan : aliased TTS.Time_Triggered_Plan :=
     (
      TT_Slot (Initial,       50 * ms, 1, HI),                                 --  #01 Seq. 1, IMs part
      TT_Slot (Initial,      350 * ms, 2, LO, (LO => 30 * ms, HI => zero)),    --  #02 Seq. 2, IMs part
      TT_Slot (Regular,      150 * ms, 3, HI, (LO => 30 * ms, HI => 50 * ms)), --  #00 Single slot for 1st seq. start
      TT_Slot (Continuation, 200 * ms, 1, HI, (LO => 30 * ms, HI => 50 * ms),
        Is_Initial => False),                                                  --  #03 Seq. 1, continuation of Ms part
      TT_Slot (Sync,         150 * ms, 1, LO, (others => 0 * ms),
        Sequence_Id => 5),                                                     --  #04 Sync Point for ET Task 1 + Empty
      TT_Slot (Terminal,     150 * ms, 1, HI, (LO => 30 * ms, HI => 80 * ms),
        Is_Initial => False),                                                  --  #05 Seq. 1, terminal of Ms part
      TT_Slot (Terminal,     100 * ms, 2, LO, (LO => 30 * ms, HI => zero),
        Is_Initial => False),                                                  --  #06 Seq. 2, terminal of Ms part
      TT_Slot (Regular,      150 * ms, 1, HI, (LO => 30 * ms, HI => 50 * ms),
        Is_Initial => False),                                                  --  #07 Seq. 1, F part
      TT_Slot (Regular,      150 * ms, 2, LO, (LO => 30 * ms, HI => zero),
        Is_Initial => False),                                                  --  #08 Seq. 2, F part
      TT_Slot (Regular,       50 * ms, 4, LO),                                 --  #09 Single slot for 2nd seq. start
      TT_Slot (Optional,      70 * ms, 5, LO, (LO => 30 * ms, HI => 0 * ms),
        Is_Initial => False),                                                  --  #10 F part of synced ET Task 1
      TT_Slot (Mode_Change,   80 * ms, Criticality => HI)                      --  #11 Mode change slot
     );

   ----------
   -- Main --
   ----------

   procedure Main is
   begin
      TTS.Set_System_Criticality_Level(LO);
      TTS.Set_System_Overrun_Handler(Criticality_Manager.Overrun_Handler'Access);
      delay until Epoch_Support.Epoch;
      TTS.Set_Plan(TT_Plan'Access);
      delay until Ada.Real_Time.Time_Last;
   end Main;

end TTS_Example_D;
