--------------------------------------------------------------------------------
--                                                                            --
--                           T T _ U T I L I T I E S                          --
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

with Ada.Real_Time; use Ada.Real_Time;

with XAda.Dispatching.TTS;

generic
   with package TTS is new XAda.Dispatching.TTS(<>);

package TT_Utilities is

   ---------------------------------------------
   --  Time_Slot kinds for building TT plans  --
   ---------------------------------------------
   type Slot_Type is (Empty,
                      Mode_Change,
                      Regular,
                      Terminal,
                      Continuation,
                      Optional,
                      Optional_Continuation,
                      Sync
                     );

   ---------------------------------------
   --  Time_Slot constructor functions  --
   ---------------------------------------
   function TT_Slot (Kind          : Slot_Type;
                     Slot_Duration : Time_Span;
                     Slot_Id       : Positive  := Positive'Last;
                     Padding       : Time_Span := Time_Span_Zero)
                     return TTS.Time_Slot_Access
   --  Make sure the Slot_Duration is non-negative and
   --  the value of Slot_Id is consistent with the kind of slot
     with Pre => ( To_Duration (Slot_Duration) >= 0.0 and then
                   ( case Kind is
                     when Empty..Mode_Change =>
                       (Slot_Id = Positive'Last),
                     when Regular..Optional_Continuation =>
                       (Slot_Id >= Positive (TTS.TT_Work_Id'First) and
                        Slot_Id <= Positive (TTS.TT_Work_Id'Last)),
                     when Sync =>
                       (Slot_Id >= Positive (TTS.TT_Sync_Id'First) and
                        Slot_Id <= Positive (TTS.TT_Sync_Id'Last)) ) );


   ---------------------------------
   --  Time_Slot setter procedure --
   ---------------------------------
   procedure Set_TT_Slot (Slot          : TTS.Time_Slot_Access;
                          Kind          : Slot_Type;
                          Slot_Duration : Time_Span;
                          Slot_Id       : Positive := Positive'Last;
                          Padding       : Time_Span := Time_Span_Zero);

end TT_Utilities;
