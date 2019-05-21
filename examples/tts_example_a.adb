with Ada.Real_Time; use Ada.Real_Time;
with TT_Utilities;
with Ada.Text_IO; use Ada.Text_IO;
with Epoch_Support; use Epoch_Support;

package body TTS_Example_A is

   Number_Of_Work_Ids : constant := 5;

   package TT_Utils is new TT_Utilities (Number_Of_Work_Ids);
   use TT_Utils;

   --  Variables incremented by two TT sequences of IMs-F tasks
   Var_1, Var_2 : Natural := 0;
   pragma Volatile (Var_1);
   pragma Volatile (Var_2);

   -- TT tasks --

   type First_Init_Task is new Simple_Task_State with null record;
   procedure Initialize (S : in out First_Init_Task) is null;
   procedure Main_Code (S : in out First_Init_Task);

   Wk1_Code : aliased First_Init_Task;
   Wk1 : Simple_TT_Task
     (Work_Id => 1,
      Task_State => Wk1_Code'Access,
      Synced_Init => False);

   type First_IMF_Task is new Initial_Mandatory_Final_Task_State with
      record
         Counter : Natural := 0;
      end record;
   procedure Initialize (S : in out First_IMF_Task) is null;
   procedure Initial_Code (S : in out First_IMF_Task);
   procedure Mandatory_Code (S : in out First_IMF_Task);
   procedure Final_Code (S : in out First_IMF_Task);

   Wk2_Code : aliased First_IMF_Task;
   Wk2 : InitialMandatorySliced_Final_TT_Task
     (Work_Id => 2,
      Task_State => Wk2_Code'Access,
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


   --  The TT plan
   TT_Plan : aliased Time_Triggered_Plan :=
     ( A_TT_Slot (Regular, 50, 1),        --  Single slot for 1st seq. start
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Regular, 50, 3),        --  Single slot for 2nd seq. start
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Regular, 20, 2),        --  Seq. 1, IMs part
       A_TT_Slot (Empty, 180),
       A_TT_Slot (Regular, 50, 4),        --  Seq. 2, IMs part
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Continuation, 20, 2),   --  Seq. 1, continuation of Ms part
       A_TT_Slot (Empty, 180),
       A_TT_Slot (Terminal, 100, 4),      --  Seq. 2, terminal of Ms part
       A_TT_Slot (Empty, 100),
       A_TT_Slot (Terminal, 20, 2),       --  Seq. 1, terminal of Ms part
       A_TT_Slot (Empty, 180),
       A_TT_Slot (Regular, 50, 4),        --  Seq. 2, F part
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Regular, 50, 2),        --  Seq. 1, F part
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Regular, 20, 5),        --  I part of end of plan
       A_TT_Slot (Empty, 80),
       A_TT_Slot (Regular, 20, 5),        --  F part of end of plan
       A_TT_Slot (Mode_Change, 80) );


   --  Auxiliary for printing times --
   function Now (Current : Time) return String is
     (Duration'Image ( To_Duration (Current - Epoch) * 1000) & " ms.");

   --  Actions of sequence initialisations
   procedure Main_Code (S : in out First_Init_Task) is  --  Simple_TT task with ID = 1
   begin
      Var_1 := 0;
      Put_Line ("First_Init_Task.Main_Code ended at " & Now (Clock));
   end Main_Code;

   procedure Main_Code (S : in out Second_Init_Task) is  --  Simple_TT task with ID = 3
   begin
      Var_2 := 0;
      Put_Line ("Second_Init_Task.Main_Code ended at " & Now (Clock));
   end Main_Code;

   --  Actions of first sequence: IMs-F task with ID = 2
   procedure Initial_Code (S : in out First_IMF_Task) is
   begin
      S.Counter := Var_1;
      Put_Line ("First_IMF_Task.Initial_Code ended at " & Now (Clock));
   end Initial_Code;

   procedure Mandatory_Code (S : in out First_IMF_Task) is
   begin
      while S.Counter < 300_000 loop
         S.Counter := S.Counter + 1;
      end loop;
      Put_Line ("First_IMF_Task.Mandatory_Code sliced ended at " & Now (Clock));
   end Mandatory_Code;

   procedure Final_Code (S : in out First_IMF_Task) is
   begin
      Var_1 := S.Counter;
      Put_Line ("First_IMF_Task.Final_Code Seq. 1 with Var_1 =" & Var_1'Image & " at" & Now (Clock));
   end Final_Code;

   --  Actions of Second sequence: IMs-F task with ID = 4
   procedure Initial_Code (S : in out Second_IMF_Task) is
   begin
      S.Counter := Var_2;
      Put_Line ("Second_IMF_Task.Initial_Code ended at " & Now (Clock));
  end Initial_Code;

   procedure Mandatory_Code (S : in out Second_IMF_Task) is
   begin
      while S.Counter < 100_000 loop
         S.Counter := S.Counter + 1;
      end loop;
      Put_Line ("Second_IMF_Task.Mandatory_Code sliced ended at " & Now (Clock));
   end Mandatory_Code;

   procedure Final_Code (S : in out Second_IMF_Task) is
   begin
      Var_2 := S.Counter;
      Put_Line ("Second_IMF_Task.Final_Code Seq. 2 with Var_2 =" & Var_2'Image  & " at" & Now (Clock));
   end Final_Code;

   --  End of plan actions: I-F task with ID = 4
   procedure Initial_Code (S : in out End_Of_Plan_IF_Task) is
   begin
      Put_Line ("Value of Var_1 =" & Var_1'Image);
      Put_Line ("Value of Var_2 =" & Var_2'Image);
      Put_Line ("Current Time =" & Now (Clock));
   end Initial_Code;

   procedure Final_Code (S : in out End_Of_Plan_IF_Task) is
   begin
      Put_Line ("Starting all over again!");
      Put_Line ("Current Time =" & Now (Clock));
   end Final_Code;

   ----------
   -- Main --
   ----------

   procedure Main is
   begin
      delay until Epoch_Support.Epoch;
      Set_Plan(TT_Plan'Access);
      delay until Ada.Real_Time.Time_Last;
   end Main;

end TTS_Example_A;
