--------------------------------------------------------------------------------
--                                                                            --
--                     S Y S T E M . T T S _ S U P P O R T                    --
--                                                                            --
--                                   S P E C                                  --
--                                                                            --
-- @author 2018-19 Jorge Real (jorge@disca.upv.es)                            --
-- @author 2018-19 Sergio Saez (ssaez@disca.upv.es)                           --
--                                                                            --
-- This GNARL extension is free software: you can redistribute it and/or      --
-- modify it under the terms of the GNU General Public License as published   --
-- by the Free Software Foundation, either version 3 of the License, or (at   --
-- your option) any later version.                                            --
--                                                                            --
-- This GNARL extension is distributed in the hope that it will be useful,    --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details.                                           --
--                                                                            --
-- You should have received a copy of the GNU General Public License along    --
-- with this program. If not, see <https://www.gnu.org/licenses/>.            --
--                                                                            --
--------------------------------------------------------------------------------

with System.BB.Threads;

package System.TTS_Support is

   pragma Preelaborate (TTS_Support);

   procedure Hold (T : System.BB.Threads.Thread_Id;
                   Check_Protected_Action : Boolean := False);
   procedure Continue (T : System.BB.Threads.Thread_Id);
   function Is_Held (T : System.BB.Threads.Thread_Id)
                   return Boolean;

end System.TTS_Support;
