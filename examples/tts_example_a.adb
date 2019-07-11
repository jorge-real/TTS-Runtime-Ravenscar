with Ada.Real_Time; use Ada.Real_Time;
with Logging_Support;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

with Epoch_Support; use Epoch_Support;

with XAda.Dispatching.TTS;
with TT_Utilities;
with TT_Patterns;

package body TTS_Example_A is

   Number_Of_Work_Ids : constant := 6;
   Number_Of_Sync_Ids : constant := 2;

   package TTS is new XAda.Dispatching.TTS
     (Number_Of_Work_Ids, Number_Of_Sync_Ids, Priority'Last - 1);

   package TT_Util is new TT_Utilities (TTS);
   use TT_Util;

   package TT_Patt is new TT_Patterns (TTS);
   use TT_Patt;

   --  Variables incremented by two TT sequences of IMs-F tasks
   Var_1, Var_2 : Natural := 0;
   pragma Volatile (Var_1);
   pragma Volatile (Var_2);

   --  Auxiliary for printing times --
   function Now (Current : Time) return String is
     (Duration'Image ( To_Duration (Current - TTS.Get_First_Plan_Release) * 1000) & " ms " &
      "|" & Duration'Image ( To_Duration (Current - TTS.Get_Last_Plan_Release) * 1000) & " ms ");

   -- TT tasks --

   type First_Init_Task is new Simple_Task_State with null record;
   procedure Initialize (S : in out First_Init_Task) is null;
   procedure Main_Code (S : in out First_Init_Task);

   Wk1_Code : aliased First_Init_Task;
   Wk1 : Simple_TT_Task
     (Work_Id => 1,
      Task_State => Wk1_Code'Access,
      Synced_Init => False);

   type First_IMF_Task is new Initial_Mandatory_Final_Task_State
     with
      record
         Counter : Natural := 0;
      end record;
   procedure Initialize (S : in out First_IMF_Task) is null;
   procedure Initial_Code (S : in out First_IMF_Task);
   procedure Mandatory_Code (S : in out First_IMF_Task);
   procedure Final_Code (S : in out First_IMF_Task);

   Wk2_State : aliased First_IMF_Task;
   Wk2 : InitialMandatorySliced_Final_TT_Task
     (Work_Id => 2,
      Task_State => Wk2_State'Access,
      Synced_Init => False);

   type Second_Init_Task is new Simple_Task_State with null record;
   procedure Initialize (S : in out Second_Init_Task) is null;
   procedure Main_Code (S : in out Second_Init_Task);

   Wk3_Code : aliased Second_Init_Task;
   Wk3 : Simple_TT_Task
     (Work_Id => 3,
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
     (Work_Id => 4,
      Task_State => Wk4_Code'Access,
      Synced_Init => False);

   type End_Of_Plan_IF_Task is new Initial_Final_Task_State with null record;
   procedure Initialize (S : in out End_Of_Plan_IF_Task) is null;
   procedure Initial_Code (S : in out End_Of_Plan_IF_Task);
   procedure Final_Code (S : in out End_Of_Plan_IF_Task);

   Wk5_Code : aliased End_Of_Plan_IF_Task;
   Wk5 : Initial_Final_TT_Task
     (Work_Id => 5,
      Task_State => Wk5_Code'Access,
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
      Work_Id => 6,
      Task_State => Wk6_Code'Access,
      Synced_Init => False);

   task type SyncedSporadic_ET_Task
     (Sync_Id : TTS.TT_Sync_Id;
      Offset  : Natural)
     with Priority => Priority'Last;

   task body SyncedSporadic_ET_Task is
      Release_Time : Time;
   begin
      loop
         TTS.Wait_For_Sync (Sync_Id, Release_Time);

         delay until Release_Time + Milliseconds (Offset);

         Put_Line ("Sporadic task interrupting at " & Now (Clock));
      end loop;
   end SyncedSporadic_ET_Task;

   Sp1 : SyncedSporadic_ET_Task
     (Sync_Id => 2, Offset => 158);

   ms : constant Time_Span := Milliseconds (1);

   --  The TT plan
   TT_Plan : aliased TTS.Time_Triggered_Plan :=
     ( TT_Slot (Regular,       50*ms, 1),  --  #00 Single slot for 1st seq. start
       TT_Slot (Empty,        150*ms   ),  --  #01
       TT_Slot (Regular,       50*ms, 3),  --  #02 Single slot for 2nd seq. start
       TT_Slot (Sync,         150*ms, 2),  --  #03 Sync point for sporadic task SP1

--         TT_Slot (Empty,         10*ms   ),

       TT_Slot (Regular,       50*ms, 2),  --  #04 Seq. 1, IMs part
       TT_Slot (Regular,       50*ms, 4),  --  #06 Seq. 2, IMs part
       TT_Slot (Empty,        150*ms   ),  --  #05
       TT_Slot (Empty,        150*ms   ),  --  #07
       TT_Slot (Continuation,  50*ms, 2),  --  #08 Seq. 1, continuation of Ms part
       TT_Slot (Empty,        150*ms   ),  --  #09
       TT_Slot (Terminal,     100*ms, 4),  --  #10 Seq. 2, terminal of Ms part
       TT_Slot (Empty,        100*ms   ),  --  #11
       TT_Slot (Terminal,      50*ms, 2),  --  #12 Seq. 1, terminal of Ms part
       TT_Slot (Sync,           0*ms, 1),  --  #13 Sync Point for ET Task 1 + Empty

       TT_Slot (Empty,        150*ms   ),

       TT_Slot (Regular,       50*ms, 4),  --  #14 Seq. 2, F part
       TT_Slot (Empty,        100*ms   ),  --  #15
       TT_Slot (Regular,       50*ms, 2),  --  #16 Seq. 1, F part
       TT_Slot (Empty,         80*ms   ),  --  #17
       TT_Slot (Regular,       50*ms, 5),  --  #18 I part of end of plan
       TT_Slot (Empty,         70*ms   ),  --  #19
       TT_Slot (Regular,       50*ms, 5),  --  #20 F part of end of plan
       TT_Slot (Optional,      70*ms, 6),  --  #21 F part of synced ET Task 1
       TT_Slot (Mode_Change,   80*ms)  );  --  #22

   --  Actions of sequence initialisations
   procedure Main_Code (S : in out First_Init_Task) is  --  Simple_TT task with ID = 1
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Var_1 := 0;
      Put_Line ("First_Init_Task.Main_Code ended at " & Now (Clock));
   end Main_Code;

   procedure Main_Code (S : in out Second_Init_Task) is  --  Simple_TT task with ID = 3
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Var_2 := 0;
      Put_Line ("Second_Init_Task.Main_Code ended at " & Now (Clock));
   end Main_Code;

   --  Actions of first sequence: IMs-F task with ID = 2
   procedure Initial_Code (S : in out First_IMF_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                 Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      S.Counter := Var_1;
      Put_Line ("First_IMF_Task.Initial_Code ended at " & Now (Clock));
   end Initial_Code;

   procedure Mandatory_Code (S : in out First_IMF_Task) is
   begin
      Put_Line ("First_IMF_Task.Mandatory_Code sliced started at " & Now (Clock));

      while S.Counter < 250_000 loop
         S.Counter := S.Counter + 1;
         if S.Counter mod 20_000 = 0 then
            Put_Line ("First_IMF_Task.Mandatory_Code sliced step " & Now (Clock));
         end if;
      end loop;

      Put_Line ("First_IMF_Task.Mandatory_Code sliced ended at " & Now (Clock));
   end Mandatory_Code;

   procedure Final_Code (S : in out First_IMF_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Var_1 := S.Counter;
      Put_Line ("First_IMF_Task.Final_Code Seq. 1 with Var_1 =" & Var_1'Image & " at" & Now (Clock));
   end Final_Code;

   --  Actions of Second sequence: IMs-F task with ID = 4
   procedure Initial_Code (S : in out Second_IMF_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      S.Counter := Var_2;
      Put_Line ("Second_IMF_Task.Initial_Code ended at " & Now (Clock));
  end Initial_Code;

   procedure Mandatory_Code (S : in out Second_IMF_Task) is
   begin
      Put_Line ("Second_IMF_Task.Mandatory_Code sliced started at " & Now (Clock));
      while S.Counter < 100_000 loop
         S.Counter := S.Counter + 1;
         if S.Counter mod 20_000 = 0 then
            Put_Line ("Second_IMF_Task.Mandatory_Code sliced step " & Now (Clock));
         end if;
      end loop;
      Put_Line ("Second_IMF_Task.Mandatory_Code sliced ended at " & Now (Clock));
   end Mandatory_Code;

   procedure Final_Code (S : in out Second_IMF_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Var_2 := S.Counter;
      Put_Line ("Second_IMF_Task.Final_Code Seq. 2 with Var_2 =" & Var_2'Image  & " at" & Now (Clock));
   end Final_Code;

   --  End of plan actions: I-F task with ID = 4
   procedure Initial_Code (S : in out End_Of_Plan_IF_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Put_Line ("End_Of_Plan_IF_Task.Initial_Code at" & Now (Clock));
      Put_Line ("Value of Var_1 =" & Var_1'Image);
      Put_Line ("Value of Var_2 =" & Var_2'Image);
   end Initial_Code;

   procedure Final_Code (S : in out End_Of_Plan_IF_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Put_Line ("End_Of_Plan_IF_Task.Final_Code at" & Now (Clock));
      New_Line;
      Put_Line ("------------------------");
      Put_Line ("Starting all over again!");
      Put_Line ("------------------------");
      New_Line;
   end Final_Code;

   procedure Initial_Code (S : in out Synced_ET_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Synced" & Integer (S.Sync_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      S.Counter := S.Counter + 1;
      Put_Line ("Synced_ET_Task.Synced_Code with counter = " & S.Counter'Image  & " at" & Now (Clock));
   end Initial_Code;

   function Final_Is_Required (S : in out Synced_ET_Task) return Boolean is
      Condition : Boolean;
   begin
      Condition := (S.Counter mod 2 = 0);
      Put_Line ("Synced_ET_Task.Final_Is_Required with condition = " & Condition'Image  & " at" & Now (Clock));
      return Condition;
   end Final_Is_Required;

   procedure Final_Code (S : in out Synced_ET_Task) is
      Jitter : Time_Span;
   begin
      --  Log --
      Jitter := Clock - S.Release_Time;
      Put_line( "Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --

      Put_Line ("Synced_ET_Task.Final_Code with counter = " & S.Counter'Image  & " at" & Now (Clock));
   end Final_Code;

   ----------
   -- Main --
   ----------

   procedure Main is
   begin
      delay until Epoch_Support.Epoch;
      TTS.Set_Plan(TT_Plan'Access);
      delay until Ada.Real_Time.Time_Last;
   end Main;

end TTS_Example_A;
