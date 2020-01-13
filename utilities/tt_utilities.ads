with Ada.Real_Time; use Ada.Real_Time;

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
                      Initial,
                      Final,
                      Terminal,
                      Continuation,
                      Optional,
                      Optional_Continuation,
                      Sync
                     );

   ---------------------------------------
   --  Time_Slot constructor functions  --
   ---------------------------------------
   function TT_Slot (Kind          : Slot_Type;
                     Slot_Duration : Time_Span;
                     Slot_Id       : Positive  := Positive'Last;
                     Criticality   : TTS.Criticality_Levels := TTS.Criticality_Levels'First;
                     Work_Duration : Time_Span := TTS.Full_Slot_Size;
                     Padding       : Time_Span := Time_Span_Zero;
                     Is_Initial    : Boolean := False)
                     return TTS.Any_Time_Slot
   --  Make sure the Slot_Duration is non-negative and
   --  the value of Slot_Id is consistent with the kind of slot
     with Pre => ( Slot_Duration >= Time_Span_Zero and then
                   ( case Kind is
                     when Empty..Mode_Change =>
                       (Slot_Id = Positive'Last),
                     when Regular..Optional_Continuation =>
                       (Slot_Id >= Positive (TTS.TT_Work_Id'First) and
                        Slot_Id <= Positive (TTS.TT_Work_Id'Last)),
                     when Sync =>
                       (Slot_Id >= Positive (TTS.TT_Sync_Id'First) and
                        Slot_Id <= Positive (TTS.TT_Sync_Id'Last)) ) );


   ---------------------------------
   --  Time_Slot setter procedure --
   ---------------------------------
   procedure Set_TT_Slot (Slot          : TTS.Any_Time_Slot;
                          Kind          : Slot_Type;
                          Slot_Duration : Time_Span;
                          Slot_Id       : Positive := Positive'Last;
                          Criticality   : TTS.Criticality_Levels := TTS.Criticality_Levels'First;
                          Work_Duration : Time_Span := TTS.Full_Slot_Size;
                          Padding       : Time_Span := Time_Span_Zero;
                          Is_Initial    : Boolean := False)
     --  Make sure the Slot_Duration is non-negative and
     --  the value of Slot_Id is consistent with the kind of slot
     with Pre => ( Slot_Duration >= Time_Span_Zero and then
                   ( case Kind is
                     when Empty..Mode_Change =>
                       (Slot_Id = Positive'Last),
                     when Regular..Optional_Continuation =>
                       (Slot_Id >= Positive (TTS.TT_Work_Id'First) and
                        Slot_Id <= Positive (TTS.TT_Work_Id'Last)),
                     when Sync =>
                       (Slot_Id >= Positive (TTS.TT_Sync_Id'First) and
                        Slot_Id <= Positive (TTS.TT_Sync_Id'Last)) ) );


end TT_Utilities;
