with Ada.Real_Time;   use Ada.Real_Time;

--  For jitter logging  --
with Logging_Support; use Logging_Support;

--  For exception tracing  --
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;

package body TT_Utilities is

   subtype Kind_Of_Slot               is TTS.Kind_Of_Slot;
   TT_Work_Slot     : Kind_Of_Slot renames TTS.TT_Work_Slot;
   Empty_Slot       : Kind_Of_Slot renames TTS.Empty_Slot;
   Mode_Change_Slot : Kind_Of_Slot renames TTS.Mode_Change_Slot;

   ---------------------------------
   --  Constructors of Time_Slots --
   ---------------------------------
   --  Auxiliary for constructing slots --
   function New_Slot  (Kind : Kind_Of_Slot;
                      MS  : Natural;
                      Work_Id : TT_Work_Id := TT_Work_Id'Last;
                      Is_Continuation : Boolean := False;
                      Is_Optional : Boolean := False) return Time_Slot;

   function A_TT_Slot (Kind : Slot_Type ;
                       Slot_Duration_MS  : Natural;
                       Work_Id : TT_Work_Id := TT_Work_Id'Last) return Time_Slot is
      Slot_Kind : Kind_Of_Slot;
      Work_Id : TT_Work_Id := TT_Work_Id'Last;
      Is_Continuation : Boolean := False;
      Is_Optional : Boolean := False;
   begin
      case Kind is
         when Empty =>
            Slot_Kind := Empty_Slot;
         when Mode_Change =>
            Slot_Kind := Mode_Change_Slot;
         when Regular | Initial | Mandatory | Mandatory_Terminal, Final | Terminal =>
            Slot_Kind := TT_Work_Slot;
         when Mandatory_Sliced, Continuation =>
            Slot_Kind := TT_Work_Slot;
            Is_Continuation := True;
         when Optional =>
            Slot_Kind := TT_Work_Slot;
            Is_Optional := True;
      end case;

      return New_Slot(Slot_Kind, Slot_Duration_MS, Is_Continuation, Is_Optional);
   end A_TT_Slot;

   function New_Slot (Kind : Kind_Of_Slot;
                      MS  : Natural;
                      Work_Id : TT_Work_Id := TT_Work_Id'Last;
                      Is_Continuation : Boolean := False;
                      Is_Optional : Boolean := False) return Time_Slot
   is
      Slot : Time_Slot (Kind);
   begin
      Slot.Slot_Duration := Ada.Real_Time.Milliseconds (MS);
      case Slot.Kind is
         when TT_Work_Slot =>
            Slot.Work_Id := Work_Id;
            Slot.Is_Continuation := Is_Continuation;
            Slot.Is_Optional := Is_Optional;
         when others =>
            null;
      end case;
      return Slot;
   end New_Slot;

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

end TT_Utilities;
