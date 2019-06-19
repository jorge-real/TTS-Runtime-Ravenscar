with Ada.Real_Time; use Ada.Real_Time;

package body TT_Patterns is

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
   

end TT_Patterns;
