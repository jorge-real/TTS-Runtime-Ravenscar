with Ada.Real_Time;   use Ada.Real_Time;

--  For jitter logging  --
with Logging_Support; use Logging_Support;

--  For exception tracing  --
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;

package body TT_Utilities is

   ---------------------------------
   --  Constructors of Time_Slots --
   ---------------------------------

   --  Auxiliary function for constructing slots --
   function A_TT_Slot (Kind : Slot_Type ;
                       Slot_Duration_MS : Natural;
                       Slot_Id : Positive := Positive'Last;
                       Padding : Time_Span := Time_Span_Zero) return Time_Slot_Access
   is
      New_Slot : Time_Slot_Access;
      Slot_Duration : Time_Span := Ada.Real_Time.Milliseconds (Slot_Duration_MS);
   begin
      case Kind is
         when Empty =>
            New_Slot := new TTS.Empty_Slot'(Slot_Duration => Slot_Duration);
         when Mode_Change =>
            New_Slot := new TTS.Mode_Change_Slot'(Slot_Duration => Slot_Duration);
         when Regular | Terminal =>
            New_Slot := new TTS.Work_Slot'(Slot_Duration => Slot_Duration,
                                           Work_Id => TT_Work_Id(Slot_Id),
                                           Is_Continuation => False,
                                           Padding => Padding);
         when Continuation =>
            New_Slot := new TTS.Work_Slot'(Slot_Duration => Slot_Duration,
                                           Work_Id => TT_Work_Id(Slot_Id),
                                           Is_Continuation => True,
                                           Padding => Padding);
         when Optional =>
            New_Slot := new TTS.Optional_Work_Slot'(Slot_Duration => Slot_Duration,
                                                    Work_Id => TT_Work_Id(Slot_Id),
                                                    Is_Continuation => False,
                                                    Padding => Padding);
         when Optional_Continuation =>
            New_Slot := new TTS.Optional_Work_Slot'(Slot_Duration => Slot_Duration,
                                                    Work_Id => TT_Work_Id(Slot_Id),
                                                    Is_Continuation => True,
                                                    Padding => Padding);
         when Sync =>
            New_Slot := new TTS.Sync_Slot'(Slot_Duration => Slot_Duration,
                                           Sync_Id => TT_Sync_Id(Slot_Id));
      end case;

      return New_Slot;
   end A_TT_Slot;

   -----------------------------------
   --  TIME-TRIGGERED TASK PATTERNS --
   -----------------------------------

   --------------------
   -- Simple_TT_Task --
   --------------------

   task body Simple_TT_Task is
      When_Was_Released : Time;
      Jitter            : Time_Span;
      Start             : Time;
   begin

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, When_Was_Released);
      end if;

      Task_State.Initialize;

      loop

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of Worker" & Integer (Work_Id)'Image &
                " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         Start := Clock;
         --  Log --

         Task_State.Main_Code;

         --  Log --
         Log (No_Event, "Work done for worker" & Integer (Work_Id)'Image);
         --  Log  --

      end loop;

   exception
      when E : others =>
         Put_Line ("Simple_TT_Task" &
                     Character'Val (Character'Pos ('0') + Integer (Work_Id)) &
                     ": " & Exception_Message (E));
   end Simple_TT_Task;

   ---------------------------
   -- Initial_Final_TT_Task --
   ---------------------------

   task body Initial_Final_TT_Task is
      When_Was_Released : Time;
      Jitter            : Time_Span;
   begin

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, When_Was_Released);
      end if;

      Task_State.Initialize;

      loop

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of I Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Initial_Code;

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of F Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Final_Code;

      end loop;

   exception
      when E : others =>
         Put_Line ("TT worker W" & Character'Val (Character'Pos ('0') + Integer (Work_Id)) &
                     ": " & Exception_Message (E));
   end Initial_Final_TT_Task;

   -------------------------------------
   -- Initial_Mandatory_Final_TT_Task --
   -------------------------------------

   task body Initial_Mandatory_Final_TT_Task is
      When_Was_Released : Time;
      Jitter            : Time_Span;
   begin

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, When_Was_Released);
      end if;

      Task_State.Initialize;

      loop

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of I Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Initial_Code;

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of M Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Mandatory_Code;

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of F Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Final_Code;

      end loop;

   exception
      when E : others =>
         Put_Line ("TT worker W" & Character'Val (Character'Pos ('0') + Integer (Work_Id)) &
                     ": " & Exception_Message (E));
   end Initial_Mandatory_Final_TT_Task;

   ------------------------------------------
   -- InitialMandatorySliced_Final_TT_Task --
   ------------------------------------------

   task body InitialMandatorySliced_Final_TT_Task is
      When_Was_Released : Time;
      Jitter            : Time_Span;
   begin

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, When_Was_Released);
      end if;

      Task_State.Initialize;

      loop

         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of I Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Initial_Code;

         TTS.Continue_Sliced;

         --  Log  --
         Log (No_Event, "Continue sliced worker" & Integer (Work_Id)'Image);
         --  Log  --

         Task_State.Mandatory_Code;

         --  Log  --
         Log (No_Event, "Mandatory sliced done for worker" & Integer (Work_Id)'Image);
         --  Log  --


         TTS.Wait_For_Activation (Work_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of F Worker" & Integer'Image (Integer (Work_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Final_Code;

         --  Log  --
         Log (No_Event, "Final part done for worker" & Integer (Work_Id)'Image);

      end loop;
   exception
      when E : others =>
         Put_Line ("TT worker W" & Character'Val (Character'Pos ('0') + Integer (Work_Id)) &
                     ": " & Exception_Message (E));
   end InitialMandatorySliced_Final_TT_Task;


   -----------------------------------------
   -- SyncedInitial_OptionalFinal_ET_Task --
   -----------------------------------------

   task body SyncedInitial_OptionalFinal_ET_Task is
      When_Was_Released : Time;
      Jitter            : Time_Span;
   begin

      if Synced_Init then
         TTS.Wait_For_Sync (Sync_Id, When_Was_Released);
      end if;

      Task_State.Initialize;

      loop

         TTS.Wait_For_Sync (Sync_Id, When_Was_Released);

         --  Log --
         Jitter := Clock - When_Was_Released;
         Log (No_Event, "|---> Jitter of P Worker" & Integer'Image (Integer (Sync_Id)) &
                       " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
         --  Log --

         Task_State.Initial_Code;

         if (Task_State.Final_Is_Required) then
            TTS.Wait_For_Activation (Work_Id, When_Was_Released);

            --  Log --
            Jitter := Clock - When_Was_Released;
            Log (No_Event, "|---> Jitter of F Worker" & Integer'Image (Integer (Work_Id)) &
                   " = " & Duration'Image (1000.0 * To_Duration (Jitter)) & " ms.");
            --  Log --

            Task_State.Final_Code;
         end if;

      end loop;

   exception
      when E : others =>
         Put_Line ("TT worker W" & Character'Val (Character'Pos ('0') + Integer (Work_Id)) &
                     ": " & Exception_Message (E));
   end SyncedInitial_OptionalFinal_ET_Task;

end TT_Utilities;
