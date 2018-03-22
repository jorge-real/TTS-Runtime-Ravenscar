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

   -- Simple Task State. Initialize + Code
   type Simple_Task_State is abstract tagged null record;
   procedure Initialize (S : in out Simple_Task_State) is abstract;
   procedure Main_Code (S : in out Simple_Task_State) is abstract;

   type Any_Simple_Task_State is access all Simple_Task_State'Class;

   -- Initial_Final Task State. Initialize + Initial_Code + Final_Code
   type Initial_Final_Task_State is abstract tagged null record;
   procedure Initialize (S : in out Initial_Final_Task_State) is abstract;
   procedure Initial_Code (S : in out Initial_Final_Task_State) is abstract;
   procedure Final_Code (S : in out Initial_Final_Task_State) is abstract;

   type Any_Initial_Final_Task_State is access all Initial_Final_Task_State'Class;

   -- Initial_Mandatory_Final Task State. Initialize + Initial_Code + Mandatory_Code + Final_Code
   type Initial_Mandatory_Final_Task_State is abstract tagged null record;
   procedure Initialize (S : in out Initial_Mandatory_Final_Task_State) is abstract;
   procedure Initial_Code (S : in out Initial_Mandatory_Final_Task_State) is abstract;
   procedure Mandatory_Code (S : in out Initial_Mandatory_Final_Task_State) is abstract;
   procedure Final_Code (S : in out Initial_Mandatory_Final_Task_State) is abstract;

   type Any_Initial_Mandatory_Final_Task_State is access all Initial_Mandatory_Final_Task_State'Class;

   ------------------------------------------------------------
   --  Time_Slot types for building patterns                 --
   ------------------------------------------------------------

   type Slot_Type is (Empty,
                      Mode_Change,
                      Regular,
                      Terminal,
                      Continuation,
                      Optional
                     );

   ------------------------------------------------------------
   --  Time_Slot constructor functions for building TT plans --
   ------------------------------------------------------------
   function A_TT_Slot (Kind : Slot_Type ;
                       Slot_Duration_MS  : Natural;
                       Work_Id : TT_Work_Id := TT_Work_Id'Last) return Time_Slot;

   -------------------------------
   --      SIMPLE TT TASK       --
   --                           --
   --  Requires 1 slot per job  --
   -------------------------------
   task type Simple_TT_Task
     (Work_Id    : TT_Work_Id;
      Task_State : Any_Simple_Task_State)
     with Priority => System.Priority'Last - 1;

   ---------------------------------
   --   INITIAL-FINAL TT TASK     --
   --                             --
   --  Requires 2 slots per job,  --
   --  one for I, and one for F   --
   ---------------------------------
   task type Initial_Final_TT_Task
     (Work_Id    : TT_Work_Id;
      Task_State : Any_Initial_Final_Task_State)
     with Priority => System.Priority'Last - 1;

   ----------------------------------------------------
   --  INITIAL - MANDATORY (sliced) - FINAL TT TASK  --
   --                                                --
   --  Requires 3 or more slots per job,             --
   --    for I, M(s) and F parts                     --
   ----------------------------------------------------
   task type Initial_Mandatory_Final_TT_Task
     (Work_Id    : TT_Work_Id;
      Task_State : Any_Initial_Mandatory_Final_Task_State)
     with Priority => System.Priority'Last - 1;

   ----------------------------------------------------
   --  INITIAL and MANDATORY sliced - FINAL TT TASK  --
   --                                                --
   --  Requires 2 slots per job, for I and F parts   --
   ----------------------------------------------------
   task type InitialMandatorySliced_Final_TT_Task
     (Work_Id    : TT_Work_Id;
      Task_State : Any_Initial_Mandatory_Final_Task_State)
     with Priority => System.Priority'Last - 1;

end TT_Utilities;
