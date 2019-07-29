--------------------------------------------------------------------------------
--                                                                            --
--                            T T _ P A T T E R N S                           --
--                                                                            --
--                                   S P E C                                  --
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

with Ada.Real_Time;
with XAda.Dispatching.TTS;

generic
   with package TTS is new XAda.Dispatching.TTS(<>);
package TT_Patterns is

   type Task_State is abstract tagged record
      Release_Time: Ada.Real_Time.Time :=  Ada.Real_Time.Time_First;
      Work_Id : TTS.TT_Work_Id;
      Sync_Id : TTS.TT_Sync_Id;
   end record;

   -- Simple Task State. Initialize + Main_Code
   type Simple_Task_State is abstract new Task_State with null record;
   procedure Initialize (S : in out Simple_Task_State) is abstract;
   procedure Main_Code (S : in out Simple_Task_State) is abstract;

   type Any_Simple_Task_State is access all Simple_Task_State'Class;

   -- Initial_Final Task State. Initialize + Initial_Code + Final_Code
   type Initial_Final_Task_State is abstract new Task_State with null record;
   procedure Initialize (S : in out Initial_Final_Task_State) is abstract;
   procedure Initial_Code (S : in out Initial_Final_Task_State) is abstract;
   procedure Final_Code (S : in out Initial_Final_Task_State) is abstract;

   type Any_Initial_Final_Task_State is access all Initial_Final_Task_State'Class;

   -- Initial_Mandatory_Final Task State. Initialize + Initial_Code + Mandatory_Code + Final_Code
   type Initial_Mandatory_Final_Task_State is abstract new Task_State with null record;
   procedure Initialize (S : in out Initial_Mandatory_Final_Task_State) is abstract;
   procedure Initial_Code (S : in out Initial_Mandatory_Final_Task_State) is abstract;
   procedure Mandatory_Code (S : in out Initial_Mandatory_Final_Task_State) is abstract;
   procedure Final_Code (S : in out Initial_Mandatory_Final_Task_State) is abstract;

   type Any_Initial_Mandatory_Final_Task_State is access all Initial_Mandatory_Final_Task_State'Class;

   -- Initial_OptionalFinal Task State. Initialize + (S)Initial_Code + [Condition] Final_Code
   type Initial_OptionalFinal_Task_State is abstract new Task_State with null record;
   procedure Initialize (S : in out Initial_OptionalFinal_Task_State) is abstract;
   procedure Initial_Code (S : in out Initial_OptionalFinal_Task_State) is abstract;
   function Final_Is_Required (S : in out Initial_OptionalFinal_Task_State) return Boolean is abstract;
   procedure Final_Code (S : in out Initial_OptionalFinal_Task_State) is abstract;

   type Any_Initial_OptionalFinal_Task_State is access all Initial_OptionalFinal_Task_State'Class;

   -------------------------------
   --      SIMPLE TT TASK       --
   --                           --
   --  Requires 1 slot per job  --
   -------------------------------
   task type Simple_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Simple_Task_State;
      Synced_Init : Boolean);

   ---------------------------------
   --   INITIAL-FINAL TT TASK     --
   --                             --
   --  Requires 2 slots per job,  --
   --  one for I, and one for F   --
   ---------------------------------
   task type Initial_Final_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_Final_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  INITIAL - MANDATORY (sliced) - FINAL TT TASK  --
   --                                                --
   --  Requires 3 or more slots per job,             --
   --    for I, M(s) and F parts                     --
   ----------------------------------------------------
   task type Initial_Mandatory_Final_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_Mandatory_Final_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  INITIAL and MANDATORY sliced - FINAL TT TASK  --
   --                                                --
   --  Requires one slot for IMs, which starts the   --
   --    sliced part, then the sliced sequence       --
   --    ending with a terminal slot, and a slot for --
   --    the F part                                  --
   ----------------------------------------------------
   task type InitialMandatorySliced_Final_TT_Task
     (Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_Mandatory_Final_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  INITIAL - [FINAL] TT TASK                     --
   --                                                --
   --  Requires one slot for I, starting the         --
   --    initial part, then ending with an optional  --
   --    slot for the final part                     --
   ----------------------------------------------------
   task type Initial_OptionalFinal_TT_Task
     (Initial_Work_Id  : TTS.TT_Work_Id;
      Optional_Work_Id : TTS.TT_Work_Id;
      Task_State       : Any_Initial_OptionalFinal_Task_State;
      Synced_Init      : Boolean);

   ------------------------------------
   --  SIMPLE SYNCED ET TASK         --
   --                                --
   --  Requires 1 sync slot per job  --
   ------------------------------------
   task type Simple_Synced_ET_Task
     (Sync_Id     : TTS.TT_Sync_Id;
      Task_State  : Any_Simple_Task_State;
      Synced_Init : Boolean);

   ----------------------------------------------------
   --  SYNC_INITIAL - [FINAL] ET TASK                --
   --                                                --
   --  Requires one sync slot for starting the       --
   --    initial part (priority-based), then ending  --
   --    with an optional slot for the final part    --
   ----------------------------------------------------
   task type SyncedInitial_OptionalFinal_ET_Task
     (Sync_Id     : TTS.TT_Sync_Id;
      Work_Id     : TTS.TT_Work_Id;
      Task_State  : Any_Initial_OptionalFinal_Task_State;
      Synced_Init : Boolean);

end TT_Patterns;
