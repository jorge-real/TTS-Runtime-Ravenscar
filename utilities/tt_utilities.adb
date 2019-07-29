--------------------------------------------------------------------------------
--                                                                            --
--                           T T _ U T I L I T I E S                          --
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

with Ada.Real_Time;   use Ada.Real_Time;

package body TT_Utilities is

   ---------------------------------
   --  Constructors of Time_Slots --
   ---------------------------------

   --  Auxiliary function for constructing slots --
   function TT_Slot (Kind          : Slot_Type ;
                     Slot_Duration : Time_Span;
                     Slot_Id       : Positive := Positive'Last;
                     Padding       : Time_Span := Time_Span_Zero)
                     return TTS.Time_Slot_Access
   is
      New_Slot : TTS.Time_Slot_Access;
   begin
      case Kind is
         when Empty =>
            New_Slot := new TTS.Empty_Slot'(Slot_Duration => Slot_Duration);
         when Mode_Change =>
            New_Slot := new TTS.Mode_Change_Slot'(Slot_Duration => Slot_Duration);
         when Regular | Terminal =>
            New_Slot := new TTS.Regular_Slot'(Slot_Duration   => Slot_Duration,
                                              Work_Id         => TTS.TT_Work_Id(Slot_Id),
                                              Is_Continuation => False,
                                              Padding         => Padding);
         when Continuation =>
            New_Slot := new TTS.Regular_Slot'(Slot_Duration   => Slot_Duration,
                                              Work_Id         => TTS.TT_Work_Id(Slot_Id),
                                              Is_Continuation => True,
                                              Padding         => Padding);
         when Optional =>
            New_Slot := new TTS.Optional_Slot'(Slot_Duration   => Slot_Duration,
                                               Work_Id         => TTS.TT_Work_Id(Slot_Id),
                                               Is_Continuation => False,
                                               Padding         => Padding);
         when Optional_Continuation =>
            New_Slot := new TTS.Optional_Slot'(Slot_Duration => Slot_Duration,
                                               Work_Id => TTS.TT_Work_Id(Slot_Id),
                                               Is_Continuation => True,
                                               Padding => Padding);
         when Sync =>
            New_Slot := new TTS.Sync_Slot'(Slot_Duration => Slot_Duration,
                                           Sync_Id => TTS.TT_Sync_Id(Slot_Id));
      end case;

      return New_Slot;
   end TT_Slot;

   procedure Set_TT_Slot (Slot          : TTS.Time_Slot_Access;
                          Kind          : Slot_Type;
                          Slot_Duration : Time_Span;
                          Slot_Id       : Positive := Positive'Last;
                          Padding       : Time_Span := Ada.Real_Time.Time_Span_Zero)
   is
   begin
      null;
   end Set_TT_Slot;

end TT_Utilities;
