with XAda.Dispatching.TTS;

generic
   with package TTS is new XAda.Dispatching.TTS(<>);
package TT_Patterns is

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

   -- Initial_OptionalFinal Task State. Initialize + (S)Initial_Code + [Condition] Final_Code
   type Initial_OptionalFinal_Task_State is abstract tagged null record;
   procedure Initialize (S : in out Initial_OptionalFinal_Task_State) is abstract;
   procedure Initial_Code (S : in out Initial_OptionalFinal_Task_State) is abstract;
   function Final_Is_Required (S : in out Initial_OptionalFinal_Task_State) return Boolean is abstract;
   procedure Final_Code (S : in out Initial_OptionalFinal_Task_State) is abstract;

   type Any_Initial_OptionalFinal_Task_State is access all Initial_OptionalFinal_Task_State'Class;

   -------------------------------
   --      SIMPLE TT TASK       --
   --                           --
   --  Requires 1 slot per job  --
   -------------------------------
   task type Simple_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Simple_Task_State;
      Synced_Init : Boolean);

   ---------------------------------
   --   INITIAL-FINAL TT TASK     --
   --                             --
   --  Requires 2 slots per job,  --
   --  one for I, and one for F   --
   ---------------------------------
   task type Initial_Final_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_Final_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  INITIAL - MANDATORY (sliced) - FINAL TT TASK  --
   --                                                --
   --  Requires 3 or more slots per job,             --
   --    for I, M(s) and F parts                     --
   ----------------------------------------------------
   task type Initial_Mandatory_Final_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_Mandatory_Final_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  INITIAL and MANDATORY sliced - FINAL TT TASK  --
   --                                                --
   --  Requires one slot for IMs, which starts the   --
   --    sliced part, then the sliced sequence       --
   --    ending with a terminal slot, and a slot for --
   --    the F part                                  --
   ----------------------------------------------------
   task type InitialMandatorySliced_Final_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_Mandatory_Final_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  SYNC_INITIAL - [FINAL] ET TASK                --
   --                                                --
   --  Requires one sync slot for starting the       --
   --    initial part (priority-based), then ending  --
   --    with an optional slot for the final part    --
   ----------------------------------------------------
   task type SyncedInitial_OptionalFinal_ET_Task
     (Sync_Id     : TTS.TT_Sync_Id;
      Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_OptionalFinal_Task_State;
      Synced_Init : Boolean);

end TT_Patterns;
