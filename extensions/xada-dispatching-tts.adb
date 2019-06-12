------------------------------------------------------------
--
--  GNAT RUN-TIME EXTENSIONS
--
--  XADA . DISPATCHING . TIME-TRIGGERED SCHEDULING
--
--  @file x-distts.adb / xada-dispatching-tts.adb
--
--  @package XAda.Dispatching.TTS (BODY)
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

with System.BB.Threads; use System.BB.Threads;
with System.TTS_Support; use System.TTS_Support;

package body XAda.Dispatching.TTS is
   
   --  Conservative bound of measured overhead on a STM32F4 Discovery
   --  Since release jitter is very predictable in this platform (between
   --  23 and 24 us) we charge that overhead at the end of the slot, by
   --  effectively advancing the slot start time by the Overhead time.
   --  This reduces the release jitter even further for TT tasks, to about 3 us
   Overhead : constant Time_Span := Microseconds (20);

   --  Run time TT work info
   type Work_Control_Block is record
      Has_Completed   : Boolean   := True;     --  TT part has completed
      Is_Waiting      : Boolean   := False;    --  Task is waiting for release
      Is_Sliced       : Boolean   := False;    --  Task is in a sliced sequence
      Work_Thread_Id  : Thread_Id := Null_Thread_Id;  --  Underlying thread id
      Last_Release    : Time      := Time_Last; -- Time of last release
   end record;

   --  Array of Work_Control_Blocks
   WCB : array (TT_Work_Id) of aliased Work_Control_Block;

   --  Array of suspension objects for TT tasks to wait for activation
   Release_Point : array (TT_Work_Id) of Suspension_Object;

   --  Run time TT sync info
   type Sync_Control_Block is record
      Sync_Thread_Id  : Thread_Id := Null_Thread_Id;  --  Underlying thread id
      Last_Release    : Time      := Time_Last; -- Time of last release
   end record;

   --  Array of Work_Control_Blocks
   SCB : array (TT_Sync_Id) of aliased Sync_Control_Block;

   --  Array of suspension objects for ET tasks to wait for synchronization
   Sync_Point : array (TT_Sync_Id) of Suspension_Object;

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
     (Work_Id : TT_Work_Id;
      When_Was_Released : out Time) is
   begin

      --  Raise own priority, before getting blocked. This is to recover the TT
      --  priority when the calling task has previuosly called Leave_TT_Level
      Set_Priority (System.Priority'Last);

      --  Inform the TT scheduler the task is going to wait for activation
      Time_Triggered_Scheduler.Prepare_For_Activation (Work_Id);

      --  Suspend until the next slot for Work_Id starts
      Suspend_Until_True (Release_Point (Work_Id));

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

   procedure Leave_TT_Level is
   begin
      Time_Triggered_Scheduler.Leave_TT_Level;
   end Leave_TT_Level;

   ---------------------------
   -- Get_Last_Plan_Release --
   ---------------------------
   
   function Get_Last_Plan_Release return Ada.Real_Time.Time is
   begin
      return Time_Triggered_Scheduler.Get_Last_Plan_Release;
   end Get_Last_Plan_Release;
   
   --------------------
   --  Wait_For_Sync --
   --------------------

   procedure Wait_For_Sync
     (Sync_Id : TT_Sync_Id;
      When_Was_Released : out Time) is
   begin
      --  Inform the TT scheduler the ET task has reached the sync point
      Time_Triggered_Scheduler.Prepare_For_Sync (Sync_Id);

      --  Suspend until the next sync slot for Sync_Id starts
      --  If the sync point has been already reached in the plan, 
      --    the SO is open and the ET task will not suspend  
      Suspend_Until_True (Sync_Point (Sync_Id));

      --  Scheduler updated Last_Release when it released the sync'ed task
      When_Was_Released := SCB (Sync_Id).Last_Release;

   end Wait_For_Sync;

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
            
            --  The extra millisecond delay is to bypass the exception we get
            --  if we don't add it. We still have to debug this. Note that the
            --  delay only affects the first mode change, because Current_Plan
            --  is null.
            Change_Plan (Now);
            
         elsif Current_Plan (Current_Slot_Index).all in Mode_Change_Slot'Class then
            
            --  Accept Set_Plan requests during a mode change slot (coming
            --  from PB tasks) and enforce the mode change at the end of it.
            Change_Plan (Next_Slot_Release);
            
         end if;

      end Set_Plan;

      ----------------------------
      -- Prepare_For_Activation --
      ----------------------------

      procedure Prepare_For_Activation (Work_Id : TT_Work_Id) is
         Current_Slot : Time_Slot_Access;
         Current_Work_Slot : Work_Slot_Access;
         Cancelled : Boolean;
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

         if Current_Plan /= null then
            Current_Slot := Current_Plan (Current_Slot_Index);
            
            if Current_Slot.all in Work_Slot'Class then
               Current_Work_Slot := Work_Slot_Access(Current_Slot);
               WCB (Current_Work_Slot.Work_Id).Has_Completed := True;
            end if;
         end if;
            
         --  Work has been completed and the caller is about to be suspended
         WCB (Work_Id).Is_Waiting := True;
         Hold_Event.Cancel_Handler(Cancelled);

         --  The task has to execute Suspend_Until_True after this point
      end Prepare_For_Activation;

      ---------------------
      -- Continue_Sliced --
      ---------------------

      procedure Continue_Sliced is
         Current_Slot : constant Time_Slot_Access :=
           Current_Plan (Current_Slot_Index);
         Current_Work_Slot : Work_Slot_Access;
      begin
         if Current_Slot.all not in Work_Slot'Class then
            raise Program_Error
              with ("Continue_Sliced called from a non-TT task");
         end if;
         
         Current_Work_Slot := Work_Slot_Access(Current_Slot);
         
         if WCB (Current_Work_Slot.Work_Id).Work_Thread_Id /= Thread_Self then
            raise Program_Error
              with ("Running Task does not correspond to Work_Id " &
                      Current_Work_Slot.Work_Id'Image);
         end if;

         WCB (Current_Work_Slot.Work_Id).Is_Sliced := True;
         
         if Current_Work_Slot.Padding > Time_Span_Zero then
            Hold_Event.Set_Handler (Next_Slot_Release - Current_Work_Slot.Padding,
                                    Hold_Handler_Access);
         end if;                        
         
      end Continue_Sliced;

      --------------------
      -- Leave_TT_Level --
      --------------------

      procedure Leave_TT_Level is -- (Work_Id : Regular_Work_Id) is
         Current_Slot : constant Time_Slot_Access :=
           Current_Plan (Current_Slot_Index);
         Current_Work_Slot : Work_Slot_Access;
         Base_Priority : System.Priority;
         Cancelled : Boolean;
      begin
         if Current_Slot.all not in Work_Slot'Class then
            raise Program_Error
              with ("Leave_TT_Level called from a non-TT task");
         end if;

         Current_Work_Slot := Work_Slot_Access(Current_Slot);
         
         if WCB (Current_Work_Slot.Work_Id).Work_Thread_Id /= Thread_Self then
            raise Program_Error
              with ("Leave_TT_Level called from Work_Id different to " &
                      Current_Work_Slot.Work_Id'Image);
         end if;

--           WCB (Work_Id).Has_Completed := True;
         WCB (Current_Work_Slot.Work_Id).Has_Completed := True;
         Hold_Event.Cancel_Handler(Cancelled);

         Base_Priority :=
           WCB (Current_Work_Slot.Work_Id).Work_Thread_Id.Base_Priority;
         Set_Priority (Base_Priority);

      end Leave_TT_Level;

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

      ---------------------------
      -- Get_Last_Plan_Release --
      ---------------------------
   
      function Get_Last_Plan_Release return Ada.Real_Time.Time is
      begin
         return First_Slot_Release;
      end Get_Last_Plan_Release;
      
      ----------------------
      -- Prepare_For_Sync --
      ----------------------

      procedure Prepare_For_Sync (Sync_Id : TT_Sync_Id) is
         Current_Slot : Time_Slot_Access;
         Current_Work_Slot : Work_Slot_Access;
      begin
         --  Register the Sync_Id with the first task using it.
         --  Use of the Sync_Id by another task breaks the model and causes PE
         if SCB (Sync_Id).Sync_Thread_Id = Null_Thread_Id then

            --  First time WFS called with this Sync_Id -> Register caller
            --  SCB (Sync_Id).Work_Task_Id := Current_Task;
            SCB (Sync_Id).Sync_Thread_Id := Thread_Self;

         elsif  SCB (Sync_Id).Sync_Thread_Id /= Thread_Self then

            --  Caller was not registered with this Sync_Id
            raise Program_Error with ("Sync_Id misuse");
         end if;

         if Current_Plan /= null then
            Current_Slot := Current_Plan (Current_Slot_Index);
            
            if Current_Slot.all in Work_Slot'Class then
               Current_Work_Slot := Work_Slot_Access(Current_Slot);
               WCB (Current_Work_Slot.Work_Id).Has_Completed := True;
            end if;
         end if;
            
         --  The task has to execute Suspend_Until_True after this point
      end Prepare_For_Sync;

      ------------------
      -- Hold_Handler --
      ------------------

      procedure Hold_Handler (Event : in out Timing_Event) is
         pragma Unreferenced (Event);
         Current_Slot : constant Time_Slot_Access :=
           Current_Plan (Current_Slot_Index);
         Current_Work_Slot : Work_Slot_Access;
         Current_WCB       : Work_Control_Block;
         Current_Thread_Id : Thread_Id;
      begin
         if Current_Slot.all not in Work_Slot'Class then
            raise Program_Error
              with ("Hold handler called for a non-TT task");
         end if;

         Current_Work_Slot := Work_Slot_Access(Current_Slot);
         
         Current_WCB := WCB (Current_Work_Slot.Work_Id);
         Current_Thread_Id := Current_WCB.Work_Thread_Id;
         
         if not Current_WCB.Has_Completed then
            Hold (Current_Thread_Id);
         end if;
         
      end Hold_Handler;
      
      ----------------
      -- NS_Handler --
      ----------------

      procedure NS_Handler (Event : in out Timing_Event) is
         pragma Unreferenced (Event);
         Current_Slot      : Time_Slot_Access;
         Current_Work_Slot : Work_Slot_Access;
         Current_WCB       : Work_Control_Block;
         Current_Sync_Slot : Sync_Slot_Access;
         Current_Thread_Id : Thread_Id;
         Now               : Time;
      begin

         --  This is the current time, according to the plan
         Now := Next_Slot_Release;

         --------------------------
         --  PROCESS ENDING SLOT --
         --------------------------

         --  Check for overrun in the ending slot, if it is a TT_Work_Slot.
         --  If this happens to be the first slot after a plan change, then
         --  we come from a mode-change slot, so there is no overrun to check,
         --  because it was checked at the start of that mode-change slot

         Current_Slot := Current_Plan (Current_Slot_Index);

         --  Nothing to be done unless the ending slot was a TT_Work_Slot
         if Current_Slot.all in Work_Slot'Class then

            Current_Work_Slot := Work_Slot_Access(Current_Slot);
            Current_WCB := WCB (Current_Work_Slot.Work_Id);

            if not Current_WCB.Has_Completed then
               --  Possible overrun detected, unless task is running sliced.
               --  First check that all is going well
               Current_Thread_Id := Current_WCB.Work_Thread_Id;

               --  Check whether the task is running sliced or this is
               --  a real overrun situation
               if Current_WCB.Is_Sliced then
                  if Current_Work_Slot.Padding > Time_Span_Zero then
                     if Current_Thread_Id.Hold_Signaled then
                        raise Program_Error
                          with ("Overrun in PA of Sliced TT task " &
                                  Current_Work_Slot.Work_Id'Image);
                     end if;
                  else 
                     --  Thread_Self is the currently running thread on this CPU.
                     --  If this assertion fails, the running TT task is using a
                     --  wrong slot, which should never happen
                     pragma Assert (Current_Thread_Id = Thread_Self);

                     Hold (Current_Thread_Id, True);
                  end if;
                  --  Context switch occurs after executing this handler

               else
                  --  Overrun detected, raise Program_Error
                  raise Program_Error 
                    with ("Overrun in TT task " &
                            Current_Work_Slot.Work_Id'Image);
               end if;

            end if;

         end if;

         ---------------------------
         -- PROCESS STARTING SLOT --
         ---------------------------

         --  Update current slot index
         Current_Slot_Index := Next_Slot_Index;
         if Current_Slot_Index = Current_Plan.all'First then
            First_Slot_Release := Now;
         end if;

         --  Obtain next slot index. The plan is repeated circularly
         if Next_Slot_Index < Current_Plan.all'Last then
            Next_Slot_Index := Next_Slot_Index + 1;
         else
            Next_Slot_Index := Current_Plan.all'First;
         end if;

         --  Obtain current slot
         Current_Slot := Current_Plan (Current_Slot_Index);

         --  Compute next slot start time
         Next_Slot_Release := Now + Current_Slot.Slot_Duration;

         if Current_Slot.all in Mode_Change_Slot'Class then
            ----------------------------------
            --  Process a Mode_Change_Slot  --
            ----------------------------------

            if Next_Plan /= null then
               --  There's a pending plan change.
               --   It takes effect at the end of the MC slot
               Change_Plan (Next_Slot_Release);
            else
               --  Set the handler for the next scheduling point
               NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                     NS_Handler_Access);
            end if;

         elsif Current_Slot.all in Empty_Slot'Class then
            -----------------------------
            --  Process an Empty_Slot  --
            -----------------------------

            --  Just set the handler for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                  NS_Handler_Access);

         elsif Current_Slot.all in Sync_Slot'Class then
            ----------------------------
            --  Process an Sync_Slot  --
            ----------------------------

            Current_Sync_Slot := Sync_Slot_Access(Current_Slot);
            SCB (Current_Sync_Slot.Sync_Id).Last_Release := Now;

            Set_True (Sync_Point (Current_Sync_Slot.Sync_Id));

            --  Set the handler for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                  NS_Handler_Access);

         elsif Current_Slot.all in Work_Slot'Class then
            -----------------------------
            --  Process a Work_Slot --
            -----------------------------

            Current_Work_Slot := Work_Slot_Access(Current_Slot);
            Current_WCB := WCB (Current_Work_Slot.Work_Id);
            Current_Thread_Id := Current_WCB.Work_Thread_Id;
            
            --  Check what needs be done to the TT task of the new slot
            if Current_WCB.Has_Completed then

               --  The TT task has abandoned the TT level or has called
               --    Wait_For_Activation

               if Current_WCB.Is_Sliced then
                  --  The completed TT task was running sliced and it has
                  --   completed, so this slot is not needed by the task.
                  null;

               elsif Current_WCB.Is_Waiting then
                  --  TT task is waiting in Wait_For_Activation

                  --  Update WCB and release TT task
                  WCB (Current_Work_Slot.Work_Id).Last_Release := Now;
                  WCB (Current_Work_Slot.Work_Id).Has_Completed := False;
                  WCB (Current_Work_Slot.Work_Id).Is_Waiting := False;
                  Set_True (Release_Point (Current_Work_Slot.Work_Id));
                     
                  if Current_Work_Slot.Is_Continuation and then
                    Current_Work_Slot.Padding > Time_Span_Zero then
                     Hold_Event.Set_Handler (Next_Slot_Release - 
                                               Current_Work_Slot.Padding,
                                             Hold_Handler_Access);
                  end if;                        

               elsif Current_Work_Slot.all in Optional_Work_Slot'Class then
                  --  If the slot is optional, it is not an error if the TT
                  --    task has not invoked Wait_For_Activation
                  null;
               else
                  --  Task is not waiting for its next activation.
                  --  It must have abandoned the TT Level or it is waiting in
                  --   a different work slot
                  raise Program_Error
                    with ("Task is late to next activation for Work_Id " &
                            Current_Work_Slot.Work_Id'Image);
               end if;

            else
               --  The TT task has not completed and no overrun has been
               --    detected so far, so it must be running sliced and is
               --    currently held from a previous exhausted slot, so it
               --    must be resumed
               pragma Assert (Current_WCB.Is_Sliced);

               --  Change thread state to runnable and insert it at the tail
               --    of its active priority, which here implies that the
               --    thread will be the next to execute
               Continue (Current_Thread_Id);

               if Current_Work_Slot.Is_Continuation and then
                 Current_Work_Slot.Padding > Time_Span_Zero then
                  Hold_Event.Set_Handler (Next_Slot_Release - 
                                            Current_Work_Slot.Padding,
                                          Hold_Handler_Access);
               end if;                        
         
            end if;

            --  Common actions to process the new slot --
            --  The work inherits its Is_Sliced condition from the
            --  Is_Continuation property of the new slot
            WCB (Current_Work_Slot.Work_Id).Is_Sliced :=
              Current_Work_Slot.Is_Continuation;

            --  Set timing event for the next scheduling point
            NS_Event.Set_Handler (Next_Slot_Release - Overhead,
                                  NS_Handler_Access);

         end if;
      end NS_Handler;

   end Time_Triggered_Scheduler;

end XAda.Dispatching.TTS;
