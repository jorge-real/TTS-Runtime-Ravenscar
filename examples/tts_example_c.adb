with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;

with Epoch_Support; use Epoch_Support;

with XAda.Dispatching.TTS;
with TT_Utilities;
with TT_Patterns;


package body TTS_Example_C is

   Number_Of_Work_Ids : constant := 4;
   Number_Of_Sync_Ids : constant := 2;

   package TTS is new XAda.Dispatching.TTS
     (Number_Of_Work_Ids, Number_Of_Sync_Ids, Priority'Last - 1);

   package TT_Util is new TT_Utilities (TTS);
   use TT_Util;

   package TT_Patt is new TT_Patterns (TTS);
   use TT_Patt;
   
   --  Auxiliary for printing absolute times with respect to plan/cycle --
   function Time_Str (T : Time) return String is
     (Duration'Image ( To_Duration (T - TTS.Get_First_Plan_Release) * 1000) & " ms " &
      "|" & Duration'Image ( To_Duration (T - TTS.Get_Last_Plan_Release) * 1000) & " ms ");
   
   
   -- TT tasks --
   
   --  Modular types to use cyclic phase counters. Each phase is a section of 
   --  the plan where we use a  particular type of ending slot to measure jitter
   --  in the next starting slot. 
   --  Ending slot types are empty, mode change, regular, sync, optional
   --  and a continuation slot that requires holding. Starting slots are regular,
   --  optional and sync.
   
   type Mod_12 is mod 12;
   type Mod_6 is mod 6;
   
   type Stats_Data is 
      record
         N      : Natural  := 0;              --  Sample size so far    
         Max    : Duration := 0.0;            --  Maximum measured value
         Min    : Duration := Duration'Last;  --  Minimum measured value
         Avg    : Duration := 0.0;            --  Average 
      end record;
   
   type Reg_Opt_Cases is array (Mod_12) of Stats_Data;
   type Sync_Cases    is array (Mod_6)  of Stats_Data;
   
            
   --  Stats data for all cases measured by task with this state.
   --  Positions 0 to 11 of the array contain the stats data for the 
   --  following cases, respectively:
   --   0 => Em_Re;  1 => Em_Op;
   --   2 => Mo_Re;  3 => Mo_Op;
   --   4 => Re_Re;  5 => Re_Op;
   --   6 => Sy_Re;  7 => Sy_Op;
   --   8 => Op_Re;  9 => Op_Op
   --  10 => Ho_Re; 11 => Ho_Op;
   --
   --  Legend: Em = Empty;    Re = Regular;      Sy = Sync; M = Mode change
   --          Op = Optional; Ho = Cont. with hold
   --
   RO_Data : Reg_Opt_Cases;
   
   --  Stats data for cases measurted by sync task
   Sy_Data : Sync_Cases;
   
   
   --  State type for the task measuring jitter of regular and optional slots
   type Reg_Opt_Task_State is new Simple_Task_State with 
      record
         --  Cyclic counter for number of activation
         Act_Counter : Mod_12 := 0;
      end record;
   procedure Initialize (S : in out Reg_Opt_Task_State) is null;
   procedure Main_Code  (S : in out Reg_Opt_Task_State);
   type Any_Reg_Opt_Task_State is access all Reg_Opt_Task_State;
   
   W1_State : aliased Reg_Opt_Task_State;
   
   --  Worker 1: measures jitter of regular and optional slots
   W1 : Simple_TT_Task
     (Work_Id => 1, Task_State => W1_State'Access, Synced_Init => False);   
   
   
   procedure Main_Code (S : in out Reg_Opt_Task_State) is
      Jitter : Duration;
      D      : Stats_Data; -- Stats data for the current phase
   begin
      Jitter := To_Duration (Clock - S.Release_Time);
      D := RO_Data (S.Act_Counter);
      --  One more sample
      D.N := D.N + 1;
      --  Calculate average
      D.Avg := (D.Avg * (D.N - 1) + Jitter) / D.N;
      --  Update Max
      if Jitter > D.Max then 
         D.Max := Jitter;
      end if;
      --  Update Min
      if Jitter < D.Min then
         D.Min := Jitter;
      end if;
      RO_Data (S.Act_Counter) := D;
      
      S.Act_Counter := S.Act_Counter + 1;
   end Main_Code;
      

   
   --  State type for auxiliary task has null initialisation and main code
   type Aux_Task_State is new Simple_Task_State with null record;
   procedure Initialize (S : in out Aux_Task_State) is null;
   procedure Main_Code (S : in out Aux_Task_State) is null;
   type Any_Aux_Task_State is access all Aux_Task_State;
   
   W2_State : aliased Aux_Task_State;
   
   --  Worker 2 is an auxiliary simple task with no actions
   W2 : Simple_TT_Task
     (Work_Id => 2, Task_State => W2_State'Access, Synced_Init => False);   


   
   type Synced_State is new Simple_Task_State with 
      record
         Act_Counter : Mod_6 := 0;
      end record;
   procedure Initialize (S : in out Synced_State) is null;
   procedure Main_Code (S: in out Synced_State);
   
   S1_State : aliased Synced_State;
   
   --  Synced task to measure jitter of sync slots
   S1 : Simple_Synced_ET_Task
     (Sync_Id => 1, Task_State => S1_State'Access, Synced_Init => False);
   
   procedure Main_Code (S : in out Synced_State) is
      Jitter : Duration;
      D      : Stats_Data;
   begin
      Jitter := To_Duration (Clock - S.Release_Time);
      D := Sy_Data (S.Act_Counter);
         --  One more sample
      D.N := D.N + 1;
      --  Calculate average
      D.Avg := (D.Avg * (D.N - 1) + Jitter) / D.N;
      --  Update Max
      if Jitter > D.Max then 
         D.Max := Jitter;
      end if;
      --  Update Min
      if Jitter < D.Min then
         D.Min := Jitter;
      end if;
      Sy_Data (S.Act_Counter) := D;
      
      S.Act_Counter := S.Act_Counter + 1;
   end Main_Code;
   
   
   S2_State: aliased Aux_Task_State;
   
   --  Synced 2 is an auxiliary Synced task with null actions
   S2 : Simple_Synced_ET_Task
     (Sync_Id => 2, Task_State => S2_State'Access, Synced_Init => False);
 
   
   
   type Aux_Sliced_Task_State is new Simple_Task_State with null record;
   procedure Initialize (S : in out Aux_Sliced_Task_State) is null;
   procedure Main_Code (S : in out Aux_Sliced_Task_State);
   
   
   W3_State : aliased Aux_Sliced_Task_State;
   
   --  Worker 3 is an auxiliary sliced task to causee "held" ending slots
   W3 : Simple_TT_Task
     (Work_Id => 3, Task_State => W3_State'Access, Synced_Init => False);   

   --  To make sliced task busy wait for a given time  
   procedure Main_Code (S : in out Aux_Sliced_Task_State) is
      --  CPU time taken by main code of task = 1.5 slots for 10 ms slots
      CPU_Interval : constant Time_Span := Milliseconds (15);
   begin
      loop
         exit when Clock - S.Release_Time >= CPU_Interval;
      end loop;
   end Main_Code;
   
   

   type Report_Task_State is new Simple_Task_State with null record;
   procedure Initialize (S : in out Report_Task_State) is null;
   procedure Main_Code (S : in out Report_Task_State);

   W4_State : aliased Report_Task_State;

   --  Worker 4 takes the final slot in the plan to print stats results
   W4 : Simple_TT_Task
     (Work_Id => 4, Task_State => W4_State'Access, Synced_Init => False);
   
   type RO_Text_Labels is array (Mod_12) of String (1..2);
   RO_Label : RO_Text_Labels := ("ER", "EO", 
                                 "MR", "MO", 
                                 "RR", "RO",
                                 "SR", "SO", 
                                 "OR", "OO",
                                 "HR", "HO");
   type Sy_Text_Labels is array (Mod_6) of String (1..2);
   Sy_Label : Sy_Text_Labels := ("ES", "MS", "RS", "SS", "OS", "HS");
                              
   --  Global max and min
   G_Max : Duration := 0.0;
   G_Min : Duration := Duration'Last;
   
   procedure Main_Code (S : in out Report_Task_State) is
      Max : Duration := 0.0;
      Min : Duration := Duration'Last;
   begin
      Put_Line (" -------- Times in milliseconds -------");
      Put_Line ("| Tr    Max         Avg         Min    |");
      Put_Line (" --------------------------------------");
      for I in Mod_12 loop
         Put (RO_Label(I) &  ":" & 
                Duration'Image (RO_Data (I).Max * 1_000.0) & 
                Duration'Image (RO_Data (I).Avg * 1_000.0) & 
                Duration'Image (RO_Data (I).Min * 1_000.0));
         New_Line;
         if RO_Data (I).Max > Max then
            Max := RO_Data (I).Max;
         end if;
         if RO_Data (I).Min < Min then
            Min := RO_Data (I).Min;
         end if;
      end loop;
      New_Line;
      for I in Mod_6 loop
         Put (Sy_Label (I) & ": " & 
                Duration'Image (Sy_Data (I).Max * 1_000.0) & 
                Duration'Image (Sy_Data (I).Avg * 1_000.0) & 
                Duration'Image (Sy_Data (I).Min * 1_000.0));
         New_Line;
      if Sy_Data (I).Max > Max then
         Max := Sy_Data (I).Max;
      end if;
      if Sy_Data (I).Min < Min then
         Min := Sy_Data (I).Min;
         end if;
      end loop;
      New_Line;
      Put_Line (" Max ="      & Duration'Image (Max * 1_000.0) &
                  "     Min =" & Duration'Image (Min * 1_000.0));
      
      if G_Max < Max then
         G_Max := Max;
      end if;
      if G_Min > Min then
         G_Min := Min;
      end if;
       Put_Line ("GMax ="      & Duration'Image (G_Max * 1_000.0) &
                  "    GMin =" & Duration'Image (G_Min * 1_000.0));
     
      Put_Line (" --------------------------------------");
      New_Line;
      New_Line;
   end Main_Code;
   

   
   
   ms : constant Time_Span := Milliseconds (1);
   
   --  The TT plan
   TT_Plan : aliased TTS.Time_Triggered_Plan :=
     ( TT_Slot (Empty,        10*ms   ),  --  #00 
       TT_Slot (Regular,      10*ms, 1),  --  #01
       TT_Slot (Empty,        10*ms   ),  --  #02 
       TT_Slot (Optional,     10*ms, 1),  --  #03
       TT_Slot (Empty,        10*ms   ),  --  #04
       TT_Slot (Sync,         10*ms, 1),  --  #05
       
       TT_Slot (Mode_Change,  10*ms   ),  --  #06 
       TT_Slot (Regular,      10*ms, 1),  --  #07
       TT_Slot (Mode_Change,  10*ms   ),  --  #08 
       TT_Slot (Optional,     10*ms, 1),  --  #09
       TT_Slot (Mode_Change,  10*ms   ),  --  #10
       TT_Slot (Sync,         10*ms, 1),  --  #11
       
       TT_Slot (Regular,      10*ms, 2),  --  #12 
       TT_Slot (Regular,      10*ms, 1),  --  #13
       TT_Slot (Regular,      10*ms, 2),  --  #14 
       TT_Slot (Optional,     10*ms, 1),  --  #15
       TT_Slot (Regular,      10*ms, 2),  --  #16 
       TT_Slot (Sync,         10*ms, 1),  --  #17
       
       TT_Slot (Sync,         10*ms, 2),  --  #18 
       TT_Slot (Regular,      10*ms, 1),  --  #19
       TT_Slot (Sync,         10*ms, 2),  --  #20 
       TT_Slot (Optional,     10*ms, 1),  --  #21
       TT_Slot (Sync,         10*ms, 2),  --  #22 
       TT_Slot (Sync,         10*ms, 1),  --  #23
       
       
       TT_Slot (Optional,     10*ms, 2),  --  #24 
       TT_Slot (Regular,      10*ms, 1),  --  #25
       TT_Slot (Optional,     10*ms, 2),  --  #26 
       TT_Slot (Optional,     10*ms, 1),  --  #27
       TT_Slot (Optional,     10*ms, 2),  --  #28 
       TT_Slot (Sync,         10*ms, 1),  --  #29
       
       TT_Slot (Continuation, 10*ms, 3),  --  #30
       TT_Slot (Regular,      10*ms, 1),  --  #31
       TT_Slot (Terminal,     10*ms, 3),  --  #32
       
       TT_Slot (Continuation, 10*ms, 3),  --  #33
       TT_Slot (Optional,     10*ms, 1),  --  #34
       TT_Slot (Terminal,     10*ms, 3),  --  #35

       TT_Slot (Continuation, 10*ms, 3),  --  #36
       TT_Slot (Sync,         10*ms, 1),  --  #37
       TT_Slot (Terminal,     10*ms, 3),  --  #38

       TT_Slot (Regular,      10*ms, 4)); --  #39

   
   procedure Main is
   begin
      delay until Epoch_Support.Epoch;
      TTS.Set_Plan(TT_Plan'Access);
      delay until Ada.Real_Time.Time_Last;
   end Main;
   
   
end TTS_Example_C;
