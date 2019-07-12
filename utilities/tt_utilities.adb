with Ada.Real_Time;   use Ada.Real_Time;

package body TT_Utilities is

   ---------------------------------
   --  Constructors of Time_Slots --
   ---------------------------------

   --  Auxiliary function for constructing slots --
   function TT_Slot (Kind          : Slot_Type ;
                     Slot_Duration : Time_Span;
                     Slot_Id       : Positive := Positive'Last;
                     Padding       : Time_Span := Time_Span_Zero)
                     return TTS.Time_Slot_Access
   is
      New_Slot : TTS.Time_Slot_Access;
   begin
      case Kind is
         when Empty =>
            New_Slot := new TTS.Empty_Slot'(Slot_Duration => Slot_Duration);
         when Mode_Change =>
            New_Slot := new TTS.Mode_Change_Slot'(Slot_Duration => Slot_Duration);
         when Regular | Terminal =>
            New_Slot := new TTS.Regular_Slot'(Slot_Duration   => Slot_Duration,
                                              Work_Id         => TTS.TT_Work_Id(Slot_Id),
                                              Is_Continuation => False,
                                              Padding         => Padding);
         when Continuation =>
            New_Slot := new TTS.Regular_Slot'(Slot_Duration   => Slot_Duration,
                                              Work_Id         => TTS.TT_Work_Id(Slot_Id),
                                              Is_Continuation => True,
                                              Padding         => Padding);
         when Optional =>
            New_Slot := new TTS.Optional_Slot'(Slot_Duration   => Slot_Duration,
                                               Work_Id         => TTS.TT_Work_Id(Slot_Id),
                                               Is_Continuation => False,
                                               Padding         => Padding);
         when Optional_Continuation =>
            New_Slot := new TTS.Optional_Slot'(Slot_Duration => Slot_Duration,
                                               Work_Id => TTS.TT_Work_Id(Slot_Id),
                                               Is_Continuation => True,
                                               Padding => Padding);
         when Sync =>
            New_Slot := new TTS.Sync_Slot'(Slot_Duration => Slot_Duration,
                                           Sync_Id => TTS.TT_Sync_Id(Slot_Id));
      end case;

      return New_Slot;
   end TT_Slot;

   procedure Set_TT_Slot (Slot          : TTS.Time_Slot_Access;
                          Kind          : Slot_Type;
                          Slot_Duration : Time_Span;
                          Slot_Id       : Positive := Positive'Last;
                          Padding       : Time_Span := Ada.Real_Time.Time_Span_Zero)
   is
   begin
      null;
   end Set_TT_Slot;

end TT_Utilities;
