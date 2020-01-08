
with System.BB.Threads.Queues;
use System.BB.Threads;
with System.Tasking;
use System.Tasking;

package body System.TTS_Support is

   procedure Suspend_Thread (Thread : System.BB.Threads.Thread_Id);
   procedure Resume_Thread (Thread : System.BB.Threads.Thread_Id);
   
   ----------
   -- Hold --
   ----------
   
   procedure Hold (T : Thread_Id;
                   Check_Protected_Action : Boolean := False) is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
      if not Is_Held (T) then
         if not In_Protected_Action(T) then
            T_Id.Common.State := Asynchronous_Hold;
            T.Hold_Signaled := False;
            Suspend_Thread (T);
         elsif Check_Protected_Action and then
           In_Protected_Action(T, 1) 
         then
            raise Program_Error with ("Hold requested in a PA");
         else
            T.Hold_Signaled := True;
         end if;
      end if;
   end Hold;
   
   --------------
   -- Continue --
   --------------
      
   procedure Continue (T : Thread_Id) is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
      pragma Assert (Is_Held (T));

      if Is_Held (T) then
         T_Id.Common.State := Runnable;
         Resume_Thread (T);
      elsif T.Hold_Signaled then
         T.Hold_Signaled := False;
      end if;

   end Continue;

   -------------
   -- Is_Held --
   -------------
   
   function Is_Held (T : Thread_Id)
                    return Boolean is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
      return (T_Id.Common.State = Asynchronous_Hold);
   end Is_Held;
   
   -------------------------
   -- In_Protected_Action --
   -------------------------
   
   function In_Protected_Action (T : Thread_Id; 
				 Level : Natural := 0) 
				return Boolean is
      T_Id : constant Task_Id := To_Task_Id (T.ATCB);
   begin
      return (T_Id.Common.Protected_Action_Nesting > Level);
   end In_Protected_Action;  
   
   
   -------------------------
   -- Internal procedures --
   -------------------------
   
   procedure Suspend_Thread (Thread : System.BB.Threads.Thread_Id) is
   begin
      Thread.State := Suspended;
      Queues.Extract (Thread);
   end Suspend_Thread;

   procedure Resume_Thread (Thread : System.BB.Threads.Thread_Id) is
   begin
      Thread.State := Runnable;
      Queues.Insert (Thread);
   end Resume_Thread;
   
   
end System.TTS_Support;
