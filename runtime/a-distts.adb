------------------------------------------------------------
--
--  ADA . DISPATCHING . TIME-TRIGGERED SCHEDULING
--
--  @file a-distts.adb
--
--  @package Ada.Dispatching.TTS (BODY)
--
--  @author Jorge Real <jorge@disca.upv.es>
--  @author Sergio Saez <ssaez@disca.upv.es>
--
------------------------------------------------------------

with Ada.Task_Identification;      use Ada.Task_Identification;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with Ada.Real_Time;                use Ada.Real_Time;
with Ada.Real_Time.Timing_Events;  use Ada.Real_Time.Timing_Events;

with Ada.Text_IO; use Ada.Text_IO;

with System.BB.Threads.Queues;

package body Ada.Dispatching.TTS is
   use System.BB.Threads;
   
   -- Lower bound of the measured overhead on a STM32F4 Discovery
   Overhead : constant Time_Span := Microseconds (20);

   --  Run time TT work info
   type Work_Control_Block is record
      Release_Point   : Suspension_Object;
      Has_Completed   : Boolean   := True;
      Is_Waiting      : Boolean   := False;
      Is_Sliced       : Boolean   := False; --  Work is on a sliced sequence
      Work_Thread_Id  : Thread_Id := Null_Thread_Id;
      Last_Release    : Time      := Time_Last;
   end record;

   --  Array of Work_Control_Blocks
   WCB : array (Regular_Work_Id) of Work_Control_Block;

   ----------------
   --  Set_Plan  --
   ----------------

   procedure Set_Plan (TTP : Time_Triggered_Plan_Access) is
   begin
      Time_Triggered_Scheduler.Set_Plan (TTP);
   end Set_Plan;

   --------------------------
   --  Wait_For_Activation --
   --------------------------
   procedure Wait_For_Activation
     (Work_Id : Regular_Work_Id;
      When_Was_Released : out Time) is
   begin

      --  It raises its own priority, before getting blocked,
      --  to recover the correct priority when this task has previuosly
      --  invoked Leave_TT_Level
      Set_Priority (System.Priority'Last);

      Time_Triggered_Scheduler.Prepare_For_Activation (Work_Id);

      --  Suspend until the next slot for Work_Id starts
      Suspend_Until_True (WCB (Work_Id).Release_Point);

      --  Scheduler updated Last_Release when it released the worker task
      When_Was_Released := WCB (Work_Id).Last_Release;

   end Wait_For_Activation;

   ---------------------
   -- Continue_Sliced --
   ---------------------

   procedure Continue_Sliced is
   begin
      Time_Triggered_Scheduler.Continue_Sliced;
   end Continue_Sliced;

   --------------------
   -- Leave_TT_Level --
   --------------------

   procedure Leave_TT_Level
     (Work_Id : Regular_Work_Id) is
      Base_Priority : System.Priority;
   begin
      Time_Triggered_Scheduler.Leave_TT_Level (Work_Id);

      Base_Priority := WCB (Work_Id).Work_Thread_Id.Base_Priority;
      Set_Priority (Base_Priority);

   end Leave_TT_Level;

   --------------------
   -- Skip_Next_Slot --
   --------------------

   procedure Skip_Next_Slot
     (Work_Id : Regular_Work_Id) is
      Must_Leave : Boolean;
   begin
      Time_Triggered_Scheduler.Skip_Next_Slot (Work_Id, Must_Leave);

      if Must_Leave then
         Leave_TT_Level (Work_Id);
      end if;
   end Skip_Next_Slot;

   ------------------------------
   -- Time_Triggered_Scheduler --
   ------------------------------

   protected body Time_Triggered_Scheduler is

      --------------
      -- Set_Plan --
      --------------

      procedure Set_Plan (TTP : Time_Triggered_Plan_Access) is
         Now : constant Time := Clock;
      begin

         --  Take note of next plan to execute
         Next_Plan := TTP;

         --  Start new plan now if none is set. Otherwise, the scheduler will
         --  change to the Next_Plan at the end of the next mode change slot
         if Current_Plan = null then
            Change_Plan (Now + Milliseconds (1));
            return;
         end if;

         --  Accept Set_Plan requests during a mode change slot (coming
         --  from ET tasks) and enforce the mode change at the end of it.
         --  Note that this point is reached only if there is currently
         --  a plan set, hence Current_Plan is not null
         if Current_Plan (Current_Slot_Index).Work_Id = Mode_Change_Slot then
            Change_Plan (Next_Slot_Release);
         end if;

      end Set_Plan;

      ----------------------------
      -- Prepare_For_Activation --
      ----------------------------

      procedure Prepare_For_Activation (Work_Id : Regular_Work_Id) is
      begin
         --  Register the Work_Id with the first task using it.
         --  Use of the Work_Id by another task breaks the model and causes PE
         if WCB (Work_Id).Work_Thread_Id = Null_Thread_Id then

            --  First time WFA called with this Work_Id -> Register caller
            --  WCB (Work_Id).Work_Task_Id := Current_Task;
            WCB (Work_Id).Work_Thread_Id := Thread_Self;

         elsif  WCB (Work_Id).Work_Thread_Id /= Thread_Self then

            --  Caller was not registered with this Work_Id
            raise Program_Error with ("Work_Id misuse");

         end if;

         --  Work has been completed and the caller is about to be suspended
         WCB (Work_Id).Is_Waiting := True;
         WCB (Work_Id).Has_Completed := True;

         --  The task has to execute Sleep after this point

      end Prepare_For_Activation;

      ---------------------
      -- Continue_Sliced --
      ---------------------

      procedure Continue_Sliced is
         Current_Work_Id : constant Any_Work_Id :=
           Current_Plan (Current_Slot_Index).Work_Id;
      begin
         if WCB (Current_Work_Id).Work_Thread_Id /= Thread_Self then
            raise Program_Error
              with ("Running Task does not correspond to Work_Id " &
                      Current_Work_Id'Image);
         end if;

         WCB (Current_Work_Id).Is_Sliced := True;
      end Continue_Sliced;

      --------------------
      -- Leave_TT_Level --
      --------------------

      procedure Leave_TT_Level (Work_Id : Regular_Work_Id) is
         Current_Work_Id : constant Any_Work_Id :=
           Current_Plan (Current_Slot_Index).Work_Id;
      begin
         if Current_Work_Id /= Work_Id then
            raise Program_Error
              with ("Incorrect Work_Id " & Work_Id'Image & " /= " &
                      Current_Work_Id'Image);
         end if;

         if WCB (Current_Work_Id).Work_Thread_Id /= Thread_Self then
            raise Program_Error
              with ("Running Task does not correspond to Work_Id " &
                      Current_Work_Id'Image);
         end if;

         WCB (Work_Id).Has_Completed := True;
      end Leave_TT_Level;

      --------------------
      -- Skip_Next_Slot --
      --------------------

      procedure Skip_Next_Slot
        (Work_Id : Regular_Work_Id;
         Must_Leave : out Boolean)
      is
         Current_Work_Id : constant Any_Work_Id :=
           Current_Plan (Current_Slot_Index).Work_Id;
      begin
         if WCB (Work_Id).Work_Thread_Id /= Thread_Self then
            raise Program_Error
              with ("Running Task does not correspond to Work_Id " &
                      Work_Id'Image);
         end if;

         Must_Leave := (Current_Work_Id = Work_Id);

         WCB (Work_Id).Is_Sliced := True;

      end Skip_Next_Slot;

      -----------------
      -- Change_Plan --
      -----------------

      procedure Change_Plan (At_Time : Time) is
      begin
         Current_Plan := Next_Plan;
         Next_Plan := null;
         --  Setting both Current_ and Next_Slot_Index to 'First is consistent
         --  with the new slot TE handler for the first slot of a new plan.
         Current_Slot_Index := Current_Plan.all'First;
         Next_Slot_Index := Current_Plan.all'First;
         Next_Slot_Release := At_Time;
         NS_Event.Set_Handler (At_Time - Overhead, NS_Handler_Access);
      end Change_Plan;

      ----------------
      -- NS_Handler --
      ----------------

      procedure NS_Handler (Event : in out Timing_Event) is
         pragma Unreferenced (Event);
         Current_Work_Id   : Any_Work_Id;
         Current_Thread_Id : Thread_Id;
         Now               : Time;
      begin
         Now := Next_Slot_Release;

         --  Check for overrun in the finishing slot.
         --  If this happens to be the first slot after a plan change, then
         --  an overrun from the last work of the old plan would have been
         --  detected at the start of the mode change slot.
         Current_Work_Id := Current_Plan (Current_Slot_Index).Work_Id;

         if Current_Work_Id in Regular_Work_Id and then
           not WCB (Current_Work_Id).Has_Completed
         then

            Current_Thread_Id := WCB (Current_Work_Id).Work_Thread_Id;

            --  Thread_Self is the currently running thread on this CPU
            if Current_Thread_Id /= Thread_Self then
               raise Program_Error
                 with ("Running Task does not correspond to Work_Id " &
                         Current_Work_Id'Image);
            end if;

            if WCB (Current_Work_Id).Is_Sliced then
               --  Action: Suspend
               --  We perform almost the same actions as in
               --    System.BB.Threads.Sleep

               --  Update status and extract from the list of ready threads
               Current_Thread_Id.State := Suspended;
               Queues.Extract (Current_Thread_Id);

               --  Context switch is performed after executing TE handler

            else
               --  Action: PE
               --  Overrun detected, raise PE
               raise Program_Error with ("Overrun detected in work " &
                                           Current_Work_Id'Image);
            end if;

         end if;

         -----------------
         -- SLOT SWITCH --
         -----------------

         --  Update current slot index
         Current_Slot_Index := Next_Slot_Index;

         --  Compute next slot index
         if Next_Slot_Index < Current_Plan.all'Last then
            Next_Slot_Index := Next_Slot_Index + 1;
         else
            Next_Slot_Index := Current_Plan.all'First;
         end if;

         --  Compute next slot start time
         Next_Slot_Release := Next_Slot_Release +
           Current_Plan (Current_Slot_Index).Slot_Duration;

         --  Obtain current work id
         Current_Work_Id := Current_Plan (Current_Slot_Index).Work_Id;

         --  Process current slot actions

         --  Mode change slot case --

         if Current_Work_Id = Mode_Change_Slot then
            if Next_Plan /= null then
               --  There's a pending plan change.
               --   It takes effect at the end of the MC slot
               Change_Plan (Next_Slot_Release);
            else
               --  Set the handler for the next scheduling point
               NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                     NS_Handler_Access);
            end if;

         --  Empty slot case  --

         elsif Current_Work_Id = Empty_Slot then
            --  Set the handler for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                  NS_Handler_Access);

         --  Regular slot case --

         elsif Current_Work_Id in Regular_Work_Id then

            Current_Thread_Id := WCB (Current_Work_Id).Work_Thread_Id;

            --  Check whether the work's task has already registered.
            --  Raise PE otherwise
            if Current_Thread_Id = Null_Thread_Id then
               raise Program_Error
                 with ("Task not registered for Work_Id " &
                         Current_Work_Id'Image);
            end if;

            --  Store in the WCN if the new starting slot is a
            --  continuation slot

            --  Release task in charge of current work id
            if WCB (Current_Work_Id).Has_Completed then

               if WCB (Current_Work_Id).Is_Sliced then
                  null;
               elsif WCB (Current_Work_Id).Is_Waiting then
                  --  Task is waiting in Wait_For_Activation

                  --  Support for release time to measure
                  --    release jitter of TT tasks

                  --  Continuation slots do not update the release time
                  WCB (Current_Work_Id).Last_Release := Now;

                  --  The worker is now released and starts running
                  WCB (Current_Work_Id).Has_Completed := False;
                  WCB (Current_Work_Id).Is_Waiting := False;

                  Set_True (WCB (Current_Work_Id).Release_Point);
               elsif Current_Plan (Current_Slot_Index).Is_Optional then
                  null;
               else
                  --  Task has invoked Leave_TT_Level
                  raise Program_Error
                    with ("Task is late to next activation for Work_Id " &
                            Current_Work_Id'Image);
               end if;

            else
               --  The thread has been suspended at the end of a previous
               --  continuation slot, so resume it and let it continue

               pragma Assert (WCB (Current_Work_Id).Is_Sliced);

               --  Update status
               Current_Thread_Id.State := Runnable;

               --  Insert the thread at the tail of its active priority
               --  so that the thread will resume execution.
               Queues.Insert (Current_Thread_Id);

            end if;

            -- Common actions --

            WCB (Current_Work_Id).Is_Sliced :=
              Current_Plan (Current_Slot_Index).Is_Continuation;

            --  Set the handler for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                  NS_Handler_Access);

         else
            raise Program_Error with "Invalid Work Id";
         end if;
      end NS_Handler;

   end Time_Triggered_Scheduler;

end Ada.Dispatching.TTS;
