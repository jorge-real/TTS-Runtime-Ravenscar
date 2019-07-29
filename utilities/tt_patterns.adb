--------------------------------------------------------------------------------
--                                                                            --
--                            T T _ P A T T E R N S                           --
--                                                                            --
--                                   B O D Y                                  --
--                                                                            --
-- @author 2018-19 Jorge Real (jorge@disca.upv.es)                            --
-- @author 2018-19 Sergio Saez (ssaez@disca.upv.es)                           --
--                                                                            --
-- This library is free software: you can redistribute it and/or modify it    --
-- under the terms of the GNU Lesser General Public License as published by   --
-- the Free Software Foundation, either version 3 of the License, or (at your --
-- option) any later version.                                                 --
--                                                                            --
-- This library is distributed in the hope that it will be useful, but        --
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public    --
-- License for more details.                                                  --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public License   --
-- along with this program. If not, see <https://www.gnu.org/licenses/>.      --
--                                                                            --
--------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;

package body TT_Patterns is

   --------------------
   -- Simple_TT_Task --
   --------------------

   task body Simple_TT_Task is
   begin

      Task_State.Work_Id := Work_Id;

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Main_Code;
      end loop;
   end Simple_TT_Task;

   ---------------------------
   -- Initial_Final_TT_Task --
   ---------------------------

   task body Initial_Final_TT_Task is
   begin

      Task_State.Work_Id := Work_Id;

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Initial_Code;

         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Final_Code;
      end loop;
   end Initial_Final_TT_Task;

   -------------------------------------
   -- Initial_Mandatory_Final_TT_Task --
   -------------------------------------

   task body Initial_Mandatory_Final_TT_Task is
   begin

      Task_State.Work_Id := Work_Id;

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Initial_Code;

         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Mandatory_Code;

         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Final_Code;
      end loop;
   end Initial_Mandatory_Final_TT_Task;

   ------------------------------------------
   -- InitialMandatorySliced_Final_TT_Task --
   ------------------------------------------

   task body InitialMandatorySliced_Final_TT_Task is
   begin

      Task_State.Work_Id := Work_Id;

      if Synced_Init then
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Initial_Code;

         TTS.Continue_Sliced;

         Task_State.Mandatory_Code;

         TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

         Task_State.Final_Code;
      end loop;
   end InitialMandatorySliced_Final_TT_Task;


   ------------------------------------
   -- Iniitial_OptionalFinal_TT_Task --
   ------------------------------------

   task body Initial_OptionalFinal_TT_Task is
   begin

      if Synced_Init then
         TTS.Wait_For_Activation (Initial_Work_Id, Task_State.Release_Time);
         Task_State.Work_Id := Initial_Work_Id;
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Activation (Initial_Work_Id, Task_State.Release_Time);
         Task_State.Work_Id := Initial_Work_Id;

         Task_State.Initial_Code;

         if (Task_State.Final_Is_Required) then
            TTS.Wait_For_Activation (Optional_Work_Id, Task_State.Release_Time);
            Task_State.Work_Id := Optional_Work_Id;

            Task_State.Final_Code;
         end if;
      end loop;
   end Initial_OptionalFinal_TT_Task;

   ---------------------------
   -- Simple_Synced_ET_Task --
   ---------------------------

   task body Simple_Synced_ET_Task is
   begin

      Task_State.Sync_Id := Sync_Id;

      if Synced_Init then
         TTS.Wait_For_Sync (Sync_Id, Task_State.Release_Time);
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Sync (Sync_Id, Task_State.Release_Time);

         Task_State.Main_Code;
      end loop;
   end Simple_Synced_ET_Task;

   -----------------------------------------
   -- SyncedInitial_OptionalFinal_ET_Task --
   -----------------------------------------

   task body SyncedInitial_OptionalFinal_ET_Task is
   begin

      Task_State.Work_Id := Work_Id;
      Task_State.Sync_Id := Sync_Id;

      if Synced_Init then
         TTS.Wait_For_Sync (Sync_Id, Task_State.Release_Time);
      end if;

      Task_State.Initialize;

      loop
         TTS.Wait_For_Sync (Sync_Id, Task_State.Release_Time);

         Task_State.Initial_Code;

         if (Task_State.Final_Is_Required) then
            TTS.Wait_For_Activation (Work_Id, Task_State.Release_Time);

            Task_State.Final_Code;
         end if;
      end loop;
   end SyncedInitial_OptionalFinal_ET_Task;

end TT_Patterns;
