with Ada.Real_Time;   use Ada.Real_Time;

package body TT_Utilities is

   ---------------------------------
   --  Constructors of Time_Slots --
   ---------------------------------

   --  Auxiliary function for constructing slots --
   function TT_Slot (Kind          : Slot_Type ;
                     Slot_Duration : Time_Span;
                     Slot_Id       : Positive := Positive'Last;
                     Work_Duration : Time_Span := TTS.Full_Slot_Size;
                     Padding       : Time_Span := Time_Span_Zero;
                     Is_Initial    : Boolean := False;
                     Is_Final      : Boolean := False)

                     return TTS.Any_Time_Slot
   is
      New_Slot : TTS.Any_Time_Slot;
   begin
      case Kind is
         when Empty =>
            New_Slot := new TTS.Empty_Slot;
         when Mode_Change =>
            New_Slot := new TTS.Mode_Change_Slot;
         when Regular | Terminal | Continuation | Initial | Final =>
            New_Slot := new TTS.Regular_Slot;
         when Optional | Optional_Continuation =>
            New_Slot := new TTS.Optional_Slot;
         when Sync =>
            New_Slot := new TTS.Sync_Slot;
      end case;

      Set_TT_Slot (New_Slot, Kind, Slot_Duration, Slot_Id, Work_Duration, Padding, Is_Initial, Is_Final);

      return New_Slot;
   end TT_Slot;

   procedure Set_Work_Slot(Slot          : TTS.Any_Work_Slot;
                           Slot_Duration : Time_Span;
                           Slot_Id       : Positive;
                           Work_Duration : Time_Span;
                           Padding       : Time_Span;
                           Continuation  : Boolean;
                           Initial       : Boolean;
                           Final         : Boolean)
   is
   begin
      Slot.Slot_Size := Slot_Duration;
      Slot.Work_Id := TTS.TT_Work_Id (Slot_Id);

      if (Work_Duration = TTS.Full_Slot_Size) then
         Slot.Work_Size := Slot_Duration;
      elsif Work_Duration <= Slot_Duration then
         Slot.Work_Size := Work_Duration;
      else
         raise Program_Error
           with ("Invalid work duration");
      end if;

      if Padding <= Slot_Duration then
         Slot.Padding_Size := Padding;
      else
         raise Program_Error
           with ("Invalid padding duration");
      end if;

      if Final and then Continuation then
         raise Program_Error
           with ("Continuation slots cannot be Final");
      end if;

      Slot.Is_Continuation := Continuation;
      Slot.Is_Initial := Initial;
      Slot.Is_Final := Final;

   end Set_Work_Slot;

   procedure Set_TT_Slot (Slot          : TTS.Any_Time_Slot;
                          Kind          : Slot_Type;
                          Slot_Duration : Time_Span;
                          Slot_Id       : Positive := Positive'Last;
                          Work_Duration : Time_Span := TTS.Full_Slot_Size;
                          Padding       : Time_Span := Ada.Real_Time.Time_Span_Zero;
                          Is_Initial    : Boolean := False;
                          Is_Final      : Boolean := False)
   is
   begin
      case Kind is
         when Empty =>
            if Slot.all not in TTS.Empty_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not an Empty slot");
            end if;

            Slot.Slot_Size := Slot_Duration;

         when Mode_Change =>
            if Slot.all not in TTS.Mode_Change_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not a Mode Change slot");
            end if;

            Slot.Slot_Size := Slot_Duration;

         when Sync =>
            if Slot.all not in TTS.Sync_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not a Sync slot");
            end if;

            Slot.Slot_Size := Slot_Duration;
            TTS.Any_Sync_Slot(Slot).Sync_Id := TTS.TT_Sync_Id(Slot_Id);

         when Regular | Terminal | Initial | Final =>
            if Slot.all not in TTS.Regular_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not a Regular slot");
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration, Slot_Id,
                           Work_Duration, Padding, False,
                           Is_Initial or (Kind = Initial), Is_Final or (Kind = Final));

         when Continuation =>
            if Slot.all not in TTS.Regular_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not a Regular slot");
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration, Slot_Id,
                           Work_Duration, Padding, True, Is_Initial, Is_Final);

         when Optional =>
            if Slot.all not in TTS.Optional_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not an Optional slot");
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration, Slot_Id,
                           Work_Duration, Padding, False, Is_Initial, Is_Final);

         when Optional_Continuation =>
            if Slot.all not in TTS.Optional_Slot'Class then
               raise Program_Error
                 with ("Provided slot is not an Optional slot");
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration, Slot_Id,
                           Work_Duration, Padding, True, Is_Initial, Is_Final);

      end case;

   end Set_TT_Slot;

end TT_Utilities;
