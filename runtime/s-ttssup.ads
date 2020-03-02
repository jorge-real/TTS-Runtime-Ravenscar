with System.BB.Threads;

package System.TTS_Support is

   pragma Preelaborate (TTS_Support);

   procedure Hold (T : System.BB.Threads.Thread_Id;
                   Check_Protected_Action : Boolean := False);
   procedure Continue (T : System.BB.Threads.Thread_Id);
   function Is_Held (T : System.BB.Threads.Thread_Id)
                    return Boolean;

   function In_Protected_Action (T : System.BB.Threads.Thread_Id;
                                 Level : Natural := 0)
                                 return Boolean;
end System.TTS_Support;
