
with System.BB.Threads.Queues;
use System.BB.Threads;
with System.Tasking;
use System.Tasking;
with System.Multiprocessors;
use System.Multiprocessors;

with System.BB.Board_Support;

package body System.TTS_Support is

   use System.BB.Board_Support.Multiprocessors;

   procedure Update_Priority (Thread : Thread_Id; Priority : Integer)
   is
      CPU_Id       : constant CPU := Current_CPU;
      Head         : Thread_Id;
      Prev_Pointer : Thread_Id;
   begin
      --  A CPU can only change the priority of its own tasks

      pragma Assert (CPU_Id = Get_CPU (Thread));

      --  Return now if there is no change. This is a rather common case, as
      --  it happens if user is not using priorities, or if the priority of
      --  an interrupt handler is the same as the priority of the interrupt.
      --  In any case, the check is quick enough.

      if Thread.Active_Priority = Priority then
         return;
      end if;

      --  Change the active priority. The base priority does not change

      Thread.Active_Priority := Priority;

      --  Outside of the executive kernel, the running thread is also the first
      --  thread in the First_Thread_Table list. This is also true in general
      --  within the kernel, except during transcient period when a task is
      --  extracted from the list (blocked by a delay until or on an entry),
      --  when a task is inserted (after a wakeup), after a yield or after
      --  this procedure. But then a context_switch put things in order.

      --  However, on ARM Cortex-M, context switches can be delayed by
      --  interrupts. They are performed via a special interrupt (Pend_SV),
      --  which is at the lowest priority. This has three consequences:
      --   A) it is not possible to have tasks in the Interrupt_Priority range
      --   B) the head of First_Thread_Table list may be different from the
      --      running thread within user interrupt handler
      --   C) the running thread may not be in the First_Thread_Table list.
      --  The following scenario shows case B: while a thread is running, an
      --  interrupt awakes a task at a higher priority; it is put in front of
      --  the First_Thread_Table queue, and a context switch is requested. But
      --  before the end of the interrupt, another interrupt triggers. It
      --  increases the priority of  the current thread, which is not the
      --  first in queue.
      --  The following scenario shows case C: a task is executing a delay
      --  until and therefore it is removed from the First_Thread_Table. But
      --  before the context switch, an interrupt triggers and change the
      --  priority of the running thread.

      --  First, find THREAD in the queue and remove it temporarly.

      Head := Queues.First_Thread_Table (CPU_Id);

      if Head = Thread then

         --  This is the very common case: THREAD is the first in the queue

         if Thread.Next = Null_Thread_Id
           or else Priority >= Thread.Next.Active_Priority
         then
            --  Already at the right place.
            return;
         end if;

         --  Remove THREAD from the queue

         Head := Thread.Next;
      else

         --  Uncommon case: less than 0.1% on a Cortex-M test.

         --  Search the thread before THREAD.

         Prev_Pointer := Head;
         loop
            if Prev_Pointer = null then
               --  THREAD is not in the queue. This corresponds to case B.
               return;
            end if;

            exit when Prev_Pointer.Next = Thread;

            Prev_Pointer := Prev_Pointer.Next;
         end loop;

         --  Remove THREAD from the queue.

         Prev_Pointer.Next := Thread.Next;
      end if;

      --  Now insert THREAD.

      --  FIFO_Within_Priorities dispatching policy. In ALRM D.2.2 it is
      --  said that when the active priority is lowered due to the loss of
      --  inherited priority (the only possible case within the Ravenscar
      --  profile) the task is added at the head of the ready queue for
      --  its new active priority.

      if Priority >= Head.Active_Priority then

         --  THREAD is the highest priority thread, so put it in the front of
         --  the queue.

         Thread.Next := Head;
         Head := Thread;
      else

         --  Search the right place in the queue.

         Prev_Pointer := Head;
         while Prev_Pointer.Next /= Null_Thread_Id
           and then Priority < Prev_Pointer.Next.Active_Priority
         loop
            Prev_Pointer := Prev_Pointer.Next;
         end loop;

         Thread.Next := Prev_Pointer.Next;
         Prev_Pointer.Next := Thread;
      end if;

      Queues.First_Thread_Table (CPU_Id) := Head;
   end Update_Priority;

   procedure Hold (T : System.BB.Threads.Thread_Id;
                   Check_Protected_Action : Boolean := False) is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
--        T.State := Suspended;
--        Queues.Extract (T);
      if not Is_Held (T) then
         if T_Id.Common.Protected_Action_Nesting = 0 then
            T_Id.Common.State := Asynchronous_Hold;
            Update_Priority (T, Held_Priority);
         elsif Check_Protected_Action then
            raise Program_Error;
         else
            T.Hold_Signaled := True;
         end if;
      end if;
   end Hold;

   procedure Continue (T : System.BB.Threads.Thread_Id) is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
--        T.State := Runnable;
--        Queues.Insert (T);
      pragma Assert (Is_Held (T));

      if Is_Held (T) then
         Update_Priority (T, T.Base_Priority);
         T_Id.Common.State := Runnable;
      elsif T.Hold_Signaled then
         T.Hold_Signaled := False;
      end if;

   end Continue;

   function Is_Held (T : System.BB.Threads.Thread_Id)
                    return Boolean is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
      return (T_Id.Common.State = Asynchronous_Hold);
   end Is_Held;

end System.TTS_Support;
