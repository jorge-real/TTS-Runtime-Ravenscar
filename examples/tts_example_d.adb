
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with System; use System;

with Epoch_Support; use Epoch_Support;

with XAda.Dispatching.TTS;
with TT_Patterns;


package body TTS_Example_D is

   Number_Of_Work_Ids : constant := 1;

   package TTS is new XAda.Dispatching.TTS
     (Number_Of_Work_IDs => Number_Of_Work_Ids,
      TT_Priority        => Priority'Last - 1);
   use TTS;


   package TT_Patt is new TT_Patterns (TTS);
   use TT_Patt;

   --  Auxiliary for printing absolute times
   function Time_Str (T : Time) return String is
     (Duration'Image (To_Duration (T - TTS.Get_First_Plan_Release) * 1000)
      & " ms ");

   --  Global for alternating duration of flexible empty slot
   type Mod_2 is mod 2;
   Factor : Mod_2 := 0;
   pragma Atomic (Factor);

   type T1_State_Type is new Simple_Task_State with null record;
   procedure Initialize (S : in out T1_State_Type) is null;
   procedure Main_Code (S: in out T1_State_Type) is
   begin
      Put_Line (Time_Str (Clock));
      Factor := Factor + 1;
   end Main_Code;

   T1_State : aliased T1_State_Type;

   Task_1 : TT_Patt.Simple_TT_Task
     (Work_Id     => 1,
      Task_State  => T1_State'Access,
      Synced_Init => False);

   --  Duration of (the only) regular slot in the plan
   Reg_Duration : constant Time_Span := Milliseconds (100);

   type Flex_Empty is new Empty_Slot with null record;
   overriding
   function Slot_Duration (S : in Flex_Empty) return Time_Span is
     (S.Default_Slot_Duration * (1 + Integer (Factor)) - Reg_Duration);

   Flex_Empty_Slot : Any_Time_Slot := new Flex_Empty'
     (Default_Slot_Duration => Seconds (1));

   Reg_Slot : Any_Time_Slot := new TTS.Regular_Slot'
     (Default_Slot_Duration   => Reg_Duration,
      Work_Id                 => 1,
      Is_Continuation         => False,
      Padding                 => Time_Span_Zero);


   --  The Time-Triggered plan
   TT_Plan : aliased TTS.Time_Triggered_Plan :=
     (Reg_Slot, Flex_Empty_Slot);


   procedure Main is
   begin
      delay until Epoch_Support.Epoch;
      TTS.Set_Plan (TT_Plan'Access);
      delay until Ada.Real_Time.Time_Last;
   exception
      when E: others =>
         Put_Line (Exception_Name (E) & "  " & Exception_Message (E));
   end Main;
end TTS_Example_D;
