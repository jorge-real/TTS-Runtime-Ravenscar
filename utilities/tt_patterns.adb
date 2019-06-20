with Ada.Real_Time; use Ada.Real_Time;

package body TT_Patterns is

   --------------------
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


   -----------------------------------------
   -- SyncedIniItial_OptionalFinal_ET_Task --
   -----------------------------------------

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
   end SyncedInitial_OptionalFinal_ET_Task;   

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
