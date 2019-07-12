with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

with Epoch_Support; use Epoch_Support;

with XAda.Dispatching.TTS;
with TT_Utilities;
with TT_Patterns;


package body TTS_Example_B is

   Number_Of_Work_Ids : constant := 7;
   Number_Of_Sync_Ids : constant := 6;

   package TTS is new XAda.Dispatching.TTS
     (Number_Of_Work_Ids, Number_Of_Sync_Ids, Priority'Last - 1);

   package TT_Util is new TT_Utilities (TTS);
   use TT_Util;

   package TT_Patt is new TT_Patterns (TTS);
   use TT_Patt;
   
   --  Auxiliary for printing times --
   function Time_Str (T : Time) return String is
     (Duration'Image ( To_Duration (T - TTS.Get_First_Plan_Release) * 1000) & " ms " &
      "|" & Duration'Image ( To_Duration (T - TTS.Get_Last_Plan_Release) * 1000) & " ms ");
   
   
   -- TT tasks --

   type Task_State is new Simple_Task_State with null record;
   procedure Initialize (S : in out Task_State) is null;
   procedure Main_Code (S : in out Task_State);
   type Any_Task_State is access all Task_State;
   
   type Cont_Task_State is new Simple_Task_State with
      record
         Counter : Integer ;
      end record;
   procedure Initialize (S : in out Cont_Task_State) is null;
   procedure Main_Code (S : in out Cont_Task_State);
   
   W1_State : aliased Task_State;
   W2_State : aliased Task_State;
   W3_State : aliased Task_State;
   W4_State : aliased Cont_Task_State;
   W5_State : aliased Task_State;
   W6_State : aliased Task_State;
   W7_State : aliased Task_State;

   W1 : Simple_TT_Task
     (Work_Id => 1,  Task_State => W1_State'Access, Synced_Init => False);
   W2 : Simple_TT_Task
     (Work_Id => 2,  Task_State => W2_State'Access, Synced_Init => False);
   W3 : Simple_TT_Task
     (Work_Id => 3,  Task_State => W3_State'Access, Synced_Init => False);
   W4 : Simple_TT_Task   --  Sliced sequence -> 1 cont. + terminal slot
     (Work_Id => 4,  Task_State => W4_State'Access, Synced_Init => False);
   W5 : Simple_TT_Task
     (Work_Id => 5,  Task_State => W5_State'Access, Synced_Init => False);
   W6 : Simple_TT_Task
     (Work_Id => 6,  Task_State => W6_State'Access, Synced_Init => False);
   W7 : Simple_TT_Task
     (Work_Id => 7,  Task_State => W7_State'Access, Synced_Init => False);

   
   procedure Main_Code (S : in out Task_State) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      Put_Line ("Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --
   end Main_Code;
   
   procedure Main_Code (S : in out Cont_Task_State) is
      Jitter : Time_Span := Clock - S.Release_Time;
   begin
      --  Log --
      Put_Line ("Worker" & Integer (S.Work_Id)'Image & " Jitter = " &
                Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
      --  Log --
      S.Counter := 0;
      for I in 1 .. 60_000 loop
         S.Counter := S.Counter + 1;
         if S.Counter mod 10_000 = 0 then
            Put_Line ("Sliced step with Count =" & S.Counter'Image &
                        " at" & Time_Str (Clock));
         end if;
      end loop;
      Put_Line ("Sliced ended at" & Time_Str (Clock));       
   end Main_Code;      
         
   --  A simple Sync task type pattern
   task type Simple_Sync_Task
     (Sync_Id : TTS.TT_Sync_Id;
      State   : Any_Task_State);
   
   task body Simple_Sync_Task is
      Jitter : Time_Span;
      Released : Time;
   begin
      State.Sync_Id := Sync_Id;

      loop
         TTS.Wait_For_Sync (State.Sync_Id, Released);
         Jitter := Clock - Released;
         --  Log --
         Put_Line ("Synced" & Integer (State.Sync_Id)'Image & " Jitter = " &
                     Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --
      end loop;
   end Simple_Sync_Task;
   
   S1_State : aliased Task_State;
   S2_State : aliased Task_State;
   S3_State : aliased Task_State;
   S4_State : aliased Task_State;
   S5_State : aliased Task_State;
   S6_State : aliased Task_State;

   S1 : Simple_Sync_Task (Sync_Id => 1, State => S1_State'Access);
   S2 : Simple_Sync_Task (Sync_Id => 2, State => S2_State'Access);
   S3 : Simple_Sync_Task (Sync_Id => 3, State => S3_State'Access);
   S4 : Simple_Sync_Task (Sync_Id => 4, State => S4_State'Access);
   S5 : Simple_Sync_Task (Sync_Id => 5, State => S5_State'Access);
   S6 : Simple_Sync_Task (Sync_Id => 6, State => S6_State'Access);

   
   ms : constant Time_Span := Milliseconds (1);
   
   --  The TT plan
   TT_Plan : aliased TTS.Time_Triggered_Plan :=
     ( TT_Slot (Empty,        10*ms   ),  --  #00 
       TT_Slot (Sync,         10*ms, 1),  --  #01
       TT_Slot (Mode_Change,  10*ms   ),  --  #02 
       TT_Slot (Sync,         10*ms, 2),  --  #03
       TT_Slot (Mode_Change,  10*ms   ),  --  #04
       
       TT_Slot (Optional,     10*ms, 1),  --  #05
       TT_Slot (Sync,         10*ms, 3),  --  #06
       TT_Slot (Sync,         10*ms, 4),  --  #07
       TT_Slot (Optional,     10*ms, 2),  --  #08
       TT_Slot (Optional,     10*ms, 3),  --  #09
       
       TT_Slot (Empty,        10*ms   ),  --  #10
       TT_Slot (Continuation, 10*ms, 4),  --  #11
       TT_Slot (Sync,         10*ms, 5),  --  #12
       TT_Slot (Terminal,     10*ms, 4),  --  #13
       TT_Slot (Regular,      10*ms, 5),  --  #14
       
       TT_Slot (Optional,     10*ms, 6),  --  #15
       TT_Slot (Continuation, 10*ms, 4),  --  #16
       TT_Slot (Optional,     10*ms, 7),  --  #17
       TT_Slot (Terminal,     10*ms, 4)); --  #18

   
   procedure Main is
   begin
      delay until Epoch_Support.Epoch;
      TTS.Set_Plan(TT_Plan'Access);
      delay until Ada.Real_Time.Time_Last;
   end Main;
   
   
end TTS_Example_B;
