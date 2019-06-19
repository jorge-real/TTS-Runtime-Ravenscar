with Ada.Real_Time;

with XAda.Dispatching.TTS;

generic

   Number_Of_Work_Ids : Positive;
   Number_Of_Sync_Ids : Positive := 1;

package TT_Utilities is

   package TTS is new XAda.Dispatching.TTS (Number_Of_Work_Ids, Number_Of_Sync_Ids);

   --  The following subtype declarations are user bypasses to the types
   --    defined in TTS, a concrete instance of XAda.Dispatching.TTS.
   --    If these were not provided here, the user code should instantiate
   --    also XAda.Dispatching.TTS with the exact same Number_Of_Work_Ids and
   --    Number_Of_Sync_Ids as in the instantiation to TT_Utilities.
   subtype TT_Work_Id                 is TTS.TT_Work_Id;
   subtype TT_Sync_Id                 is TTS.TT_Sync_Id;
   subtype Time_Slot                  is TTS.Time_Slot;
   subtype Time_Slot_Access           is TTS.Time_Slot_Access;
   subtype Time_Triggered_Plan        is TTS.Time_Triggered_Plan;
   subtype Time_Triggered_Plan_Access is TTS.Time_Triggered_Plan_Access;

   --  Ditto for procedure Set_Plan
   procedure Set_Plan (TTP : Time_Triggered_Plan_Access) renames TTS.Set_Plan;
   --  Ditto for function Get_last_Plan_Release
   function Get_Last_Plan_Release return Ada.Real_Time.Time renames TTS.Get_Last_Plan_Release;

   ---------------------------------------------
   --  Time_Slot kinds for building TT plans  --
   ---------------------------------------------
   type Slot_Type is (Empty,
                      Mode_Change,
                      Regular,
                      Terminal,
                      Continuation,
                      Optional,
                      Optional_Continuation,
                      Sync
                     );

   ------------------------------------------------------------
   --  Time_Slot constructor functions for building TT plans --
   ------------------------------------------------------------
   function A_TT_Slot (Kind             : Slot_Type ;
                       Slot_Duration_MS : Natural;
                       Slot_Id          : Positive := Positive'Last;
                       Padding          : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
                       return Time_Slot_Access;

end TT_Utilities;
