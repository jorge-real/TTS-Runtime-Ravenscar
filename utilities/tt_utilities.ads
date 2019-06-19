with Ada.Real_Time;

with XAda.Dispatching.TTS;

generic
   with package TTS is new XAda.Dispatching.TTS(<>);

package TT_Utilities is

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
                       return TTS.Time_Slot_Access;

end TT_Utilities;
