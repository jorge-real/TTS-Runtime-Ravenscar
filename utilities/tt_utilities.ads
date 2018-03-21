with Ada.Dispatching.TTS;
with System;

generic

   Number_Of_Work_Ids : Positive;

package TT_Utilities is

   package TTS is new Ada.Dispatching.TTS (Number_Of_Work_Ids);
   --  use TTS;

   --  The following subtype declarations are user bypasses to the types
   --    defined in TTS, a concrete instance of Ada.Dispatching.TTS.
   --    If these were not provided here, the user code should instantiate
   --    also Ada.Dispatching.TTS with the exact same Number_Of_Work_Ids
   --    as in the instantiation to TTS_Patterns.

   subtype TT_Work_Id                 is TTS.TT_Work_Id;
   subtype Time_Slot                  is TTS.Time_Slot;
   subtype Time_Triggered_Plan        is TTS.Time_Triggered_Plan;
   subtype Time_Triggered_Plan_Access is TTS.Time_Triggered_Plan_Access;

      --  Ditto for procedure Set_Plan
   procedure Set_Plan (TTP : Time_Triggered_Plan_Access) renames TTS.Set_Plan;

   --  Access type to parameterless action procedures
   type Task_Actions is not null access procedure;

   ------------------------------------------------------------
   --  Time_Slot types for building patterns                 --
   ------------------------------------------------------------

   type Slot_Type is (Empty,
                      Mode_Change,
                      Regular,
                      Initial,
                      Mandatory,
                      Final,
                      Continuation,
                      Terminal,
                      Optional
                     );

   ------------------------------------------------------------
   --  Time_Slot constructor functions for building TT plans --
   ------------------------------------------------------------
   function A_TT_Work_Slot (Slot_Duration_MS  : Natural;
                            Work_Id : TT_Work_Id := TT_Work_Id'Last;
                            Is_Continuation : Boolean := False;
                            Is_Optional : Boolean := False) return Time_Slot;

   function An_Empty_Slot (Slot_Duration_MS  : Natural) return Time_Slot;

   function A_Mode_Change_Slot (Slot_Duration_MS  : Natural) return Time_Slot;

   function New_Slot (Kind : Slot_Type ;
                      Slot_Duration_MS  : Natural;
                      Work_Id : TT_Work_Id :) return Time_Slot;

   -------------------------------
   --      SIMPLE TT TASK       --
   --                           --
   --  Requires 1 slot per job  --
   -------------------------------
   task type Simple_TT_Task
     (Work_Id : TT_Work_Id;
      Actions : Task_Actions)
     with Priority => System.Priority'Last - 1;

   ---------------------------------
   --   INITIAL-FINAL TT TASK     --
   --                             --
   --  Requires 2 slots per job,  --
   --  one for I, and one for F   --
   ---------------------------------
   task type Initial_Final_TT_Task
     (Work_Id      : TT_Work_Id;
      Initial_Part : Task_Actions;
      Final_Part   : Task_Actions)
     with Priority => System.Priority'Last - 1;

   ----------------------------------------------------
   --  INITIAL and MANDATORY sliced - FINAL TT TASK  --
   --                                                --
   --  Requires 2 slots per job, for I and F parts   --
   ----------------------------------------------------
   task type InitialMandatorySliced_Final_TT_Task
     (Work_Id               : TT_Work_Id;
      Initial_Part          : Task_Actions;
      Mandatory_Sliced_Part : Task_Actions;
      Final_Part            : Task_Actions)
     with Priority => System.Priority'Last - 1;

end TT_Utilities;
