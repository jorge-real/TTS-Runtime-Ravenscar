with Ada.Real_Time;   use Ada.Real_Time;

package body TT_Utilities is

   ---------------------------------
   --  Constructors of Time_Slots --
   ---------------------------------

   --  Auxiliary function for constructing slots --
   function TT_Slot (Kind           : Slot_Type ;
                     Slot_Duration  : Time_Span;
                     Slot_Id        : Positive := TTS.No_Id;
                     Criticality    : TTS.Criticality_Levels := TTS.Criticality_Levels'First;
                     Work_Durations : TTS.Time_Span_Array := (others => TTS.Full_Slot_Size);
                     Paddings       : TTS.Time_Span_Array := (others => Time_Span_Zero);
                     Is_Initial     : Boolean := True;
                     Sequence_Id    : Positive := TTS.No_Id -- Only for Sync slots
                    )

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

      Set_TT_Slot (New_Slot, Kind, Slot_Duration,
                   Slot_Id, Criticality, Work_Durations, Paddings, Is_Initial, Sequence_Id);

      return New_Slot;
   end TT_Slot;

   procedure Set_Work_Slot (Slot           : TTS.Any_Work_Slot;
                            Slot_Duration  : Time_Span;
                            Slot_Id        : Positive;
                            Criticality    : TTS.Criticality_Levels;
                            Work_Durations : TTS.Time_Span_Array;
                            Paddings       : TTS.Time_Span_Array;
                            Continuation   : Boolean;
                            Initial        : Boolean)
   is
   begin
      Slot.Slot_Size := Slot_Duration;
      begin
         Slot.Work_Id := TTS.TT_Work_Id (Slot_Id);
      exception
         when Constraint_Error =>
            raise TTS.Plan_Error
            with "Invalid work Id" & Slot_Id'Image;
      end;

      for I in Work_Durations'Range loop
         if (Work_Durations (I) = TTS.Full_Slot_Size) then
            Slot.Work_Sizes (I) := Slot_Duration;
         elsif Work_Durations (I) <= Slot_Duration then
            Slot.Work_Sizes (I) := Work_Durations (I);
         else
            raise TTS.Plan_Error
              with ("Invalid work duration (" & I'Image & ")");
         end if;

         if Paddings (I) <= Slot.Work_Sizes (I) then
            Slot.Padding_Sizes (I) := Paddings (I);
         else
            raise TTS.Plan_Error
              with ("Invalid padding duration (" & I'Image & ")");
         end if;

      end loop;

      Slot.Criticality_Level := Criticality;
      Slot.Is_Continuation := Continuation;
      Slot.Is_Initial := Initial;

   end Set_Work_Slot;

   procedure Set_TT_Slot (Slot           : TTS.Any_Time_Slot;
                          Kind           : Slot_Type;
                          Slot_Duration  : Time_Span;
                          Slot_Id        : Positive := TTS.No_Id;
                          Criticality    : TTS.Criticality_Levels := TTS.Criticality_Levels'First;
                          Work_Durations : TTS.Time_Span_Array := (others => TTS.Full_Slot_Size);
                          Paddings       : TTS.Time_Span_Array := (others => Time_Span_Zero);
                          Is_Initial     : Boolean := True;
                          Sequence_Id    : Positive := TTS.No_Id -- Only for Sync slots
                         )
   is
      Sync_Slot : TTS.Any_Sync_Slot;
   begin
      case Kind is
         when Empty =>
            if Slot.all not in TTS.Empty_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not an Empty slot";
            end if;

            Slot.Slot_Size := Slot_Duration;

         when Mode_Change =>
            if Slot.all not in TTS.Mode_Change_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not a Mode Change slot";
            end if;

            Slot.Slot_Size := Slot_Duration;
            Slot.Criticality_Level := Criticality;

         when Sync =>
            if Slot.all not in TTS.Sync_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not a Sync slot";
            end if;

            Sync_Slot := TTS.Any_Sync_Slot (Slot);

            Sync_Slot.Slot_Size := Slot_Duration;
            Sync_Slot.Criticality_Level := Criticality;
            begin
               Sync_Slot.Sync_Id := TTS.TT_Sync_Id (Slot_Id);
            exception
               when Constraint_Error =>
                  raise TTS.Plan_Error
                  with "Invalid sync Id" & Slot_Id'Image;
            end;

            Sync_Slot.Is_Initial := Is_Initial;
            Sync_Slot.In_Work_Sequence := (Sequence_Id /= TTS.No_Id);
            if Sync_Slot.In_Work_Sequence then
               begin
                  Sync_Slot.Work_Id := TTS.TT_Work_Id (Sequence_Id);
               exception
                  when Constraint_Error =>
                     raise TTS.Plan_Error
                     with "Invalid work Id" & Slot_Id'Image;
               end;
            end if;

         when Regular | Terminal | Initial | Final =>
            if Slot.all not in TTS.Regular_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not a Regular slot";
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration,
                           Slot_Id, Criticality, Work_Durations, Paddings, False,
                           Is_Initial or (Kind = Initial));

         when Continuation =>
            if Slot.all not in TTS.Regular_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not a Regular slot";
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration,
                           Slot_Id, Criticality, Work_Durations, Paddings, True, Is_Initial);

         when Optional =>
            if Slot.all not in TTS.Optional_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not an Optional slot";
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration,
                           Slot_Id, Criticality, Work_Durations, Paddings, False, Is_Initial);

         when Optional_Continuation =>
            if Slot.all not in TTS.Optional_Slot'Class then
               raise TTS.Plan_Error with
                 "Provided slot is not an Optional slot";
            end if;

            Set_Work_Slot (TTS.Any_Work_Slot (Slot), Slot_Duration,
                           Slot_Id, Criticality, Work_Durations, Paddings, True, Is_Initial);

      end case;

   end Set_TT_Slot;

end TT_Utilities;
