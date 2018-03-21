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

   --  TT actions
   procedure First_Seq_Start;
   procedure Second_Seq_Start;
   procedure First_Initial;
   procedure First_Mandatory_Sliced;
   procedure First_Final;
   procedure Second_Initial;
   procedure Second_Mandatory_Sliced;
   procedure Second_Final;
   procedure End_Of_Plan_Initial;
   procedure End_Of_Plan_Final;

   --  TT tasks --
   Wk1 : Simple_TT_Task (Work_Id => 1,
                         Actions => First_Seq_Start'Access);

   Wk2 : InitialMandatorySliced_Final_TT_Task
     (Work_Id => 2,
      Initial_Part => First_Initial'Access,
      Mandatory_Sliced_Part => First_Mandatory_Sliced'Access,
      Final_Part => First_Final'Access);

   Wk3 : Simple_TT_Task (Work_Id => 3,
                         Actions => Second_Seq_Start'Access);
   Wk4 : InitialMandatorySliced_Final_TT_Task
     (Work_Id => 4,
      Initial_Part => Second_Initial'Access,
      Mandatory_Sliced_Part => Second_Mandatory_Sliced'Access,
      Final_Part => Second_Final'Access);

   Wk5 : Initial_Final_TT_Task
     (Work_Id => 5,
      Initial_Part => End_Of_Plan_Initial'Access,
      Final_Part => End_Of_Plan_Final'Access);


   --  The TT plan
   TT_Plan : aliased Time_Triggered_Plan :=
     ( A_TT_Slot (Regular, 50, 1),        --  Single slot for 1st seq. start
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Regular, 50, 3),        --  Single slot for 2nd seq. start
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Initial, 20, 2),        --  Seq. 1, IMs part
       A_TT_Slot (Empty, 180),
       A_TT_Slot (Initial, 50, 4),        --  Seq. 2, IMs part
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Mandatory_Sliced, 20, 2),    --  Seq. 1, continuation of Ms part
       A_TT_Slot (Empty, 180),
       A_TT_Slot (Mandatory_Terminal, 100, 4), --  Seq. 2, terminal of Ms part
       A_TT_Slot (Empty, 100),
       A_TT_Slot (Mandatory_Terminal, 20, 2),  --  Seq. 1, terminal of Ms part
       A_TT_Slot (Empty, 180),
       A_TT_Slot (Final, 50, 4),          --  Seq. 2, F part
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Final, 50, 2),          --  Seq. 1, F part
       A_TT_Slot (Empty, 150),
       A_TT_Slot (Initial, 20, 5),        --  I part of end of plan
       A_TT_Slot (Empty, 80),
       A_TT_Slot (Final, 20, 5),          --  F part of end of plan
       A_TT_Slot (Mode_Change, 80) );

   --  Auxiliary for printing times --
   function Now (Current : Time) return String is
     (Duration'Image ( To_Duration (Current - Epoch) * 1000) & " ms.");

   --  Actions of sequence initialisations
   procedure First_Seq_Start is  --  Simple_TT task with ID = 1
   begin
      Var_1 := 0;
      Put_Line ("First_Seq_Start ended at " & Now (Clock));
   end First_Seq_Start;

   procedure Second_Seq_Start is  --  Simple_TT task with ID = 3
   begin
      Var_2 := 0;
      Put_Line ("Second_Seq_Start ended at " & Now (Clock));
   end Second_Seq_Start;

   --  Actions of first sequence: IMs-F task with ID = 2
   procedure First_Initial is
   begin
      Var_1 := 1;
      Put_Line ("First_Initial ended at " & Now (Clock));
   end First_Initial;

   procedure First_Mandatory_Sliced is
   begin
      while Var_1 < 300_000 loop
         Var_1 := Var_1 + 1;
      end loop;
      Put_Line ("First_Mandatory_Sliced ended at " & Now (Clock));
   end First_Mandatory_Sliced;

   procedure First_Final is
   begin
      Put_Line ("End of Seq. 1 with Var_1 =" & Var_1'Image & " at" & Now (Clock));
   end First_Final;


   --  Actions of Second sequence: IMs-F task with ID = 4
   procedure Second_Initial is
   begin
      Var_2 := 1;
      Put_Line ("Second_Initial ended at " & Now (Clock));
  end Second_Initial;

   procedure Second_Mandatory_Sliced is
   begin
      while Var_2 < 100_000 loop
         Var_2 := Var_2 + 1;
      end loop;
      Put_Line ("Second_Mandatory_Sliced ended at " & Now (Clock));
   end Second_Mandatory_Sliced;

   procedure Second_Final is
   begin
      Put_Line ("End of Seq. 2 with Var_2 =" & Var_2'Image  & " at" & Now (Clock));
   end Second_Final;

   --  End of plan actions: I-F task with ID = 4
   procedure End_Of_Plan_Initial is
   begin
      Put_Line ("Value of Var_1 =" & Var_1'Image);
      Put_Line ("Value of Var_2 =" & Var_2'Image);
      Put_Line ("Current Time =" & Now (Clock));
   end End_Of_Plan_Initial;

   procedure End_Of_Plan_Final is
   begin
      Put_Line ("Starting all over again!");
      Put_Line ("Current Time =" & Now (Clock));
   end End_Of_Plan_Final;

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
