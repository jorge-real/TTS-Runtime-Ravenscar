```ada
   --  Set new TT plan to start at the end of the next mode change slot
   procedure Set_Plan
     (TTP : Time_Triggered_Plan_Access);

   --  TT works use this procedure to wait for their next assigned slot
   --  The When_Was_Released result informs caller of slot starting time
   procedure Wait_For_Activation
     (Work_Id           : TT_Work_Id;
      When_Was_Released : out Ada.Real_Time.Time);

   --  TT works use this procedure to inform that the critical part
   --  of the current slot has been finished. It tranforms the current
   --  slot in a continuation slot
   procedure Continue_Sliced;

   --  TT works use this procedure to inform the TT scheduler that
   --   there is no more work to do at TT priority level
   procedure Leave_TT_Level;

   --  Returns the last time the first slot of the plan was released
   function Get_Last_Plan_Release return Ada.Real_Time.Time;

   --  ET works use this procedure to wait for their next asigned sync slot
   procedure Wait_For_Sync
     (Sync_Id           : TT_Sync_Id;
      When_Was_Released : out Ada.Real_Time.Time);
```