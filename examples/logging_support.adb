--------------------------------------------------------------------------------
--                                                                            --
--                        L O G G I N G _ S U P P O R T                       --
--                                                                            --
--                                   B O D Y                                  --
--                                                                            --
-- @author 2018-19 Jorge Real (jorge@disca.upv.es)                            --
-- @author 2018-19 Sergio Saez (ssaez@disca.upv.es)                           --
--                                                                            --
-- This program is free software: you can redistribute it and/or modify it    --
-- under the terms of the GNU General Public License as published by the Free --
-- Software Foundation, either version 3 of the License, or (at your option)  --
-- any later version.                                                         --
--                                                                            --
-- This program is distributed in the hope that it will be useful, but        --
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   --
-- for more details.                                                          --
--                                                                            --
-- You should have received a copy of the GNU General Public License along    --
-- with this program. If not, see <https://www.gnu.org/licenses/>.            --
--                                                                            --
--------------------------------------------------------------------------------

--  pragma Locking_Policy (Ceiling_Locking);

with Ada.Text_IO;
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Epoch_Support;           use Epoch_Support;

with System;

package body Logging_Support is
   ------------------------------------------------------------------------
   --  | Body of Package for Logging tasks activity
   --  | WITH-ed by the body of a package to provide an easy way to
   --  | trace events related to task activation and completion
   --  | Implemented as a modification to the Debugging_Support package
   --  | from Michael B. Feldman, The George Washington University
   --  | Author: Jorge Real
   --  | Last Modified: December 2017
   --  |                  - Adapted to STM32 board - no file output
   --  |                  - Removed procedure Set_Log
   --  |                  - Use of Epoch_Support
   --  |                November 2014
   --  |                  - Added PO for serialised output
   -------------------------------------------------------------------------

   Two_Tabs : constant String := HT & HT;

   Init : constant Time := Epoch;


   protected Serialise
     with Priority => System.Interrupt_Priority'Last
   is
      procedure Log (Event : in Event_Type; Message : in String := "");
   end Serialise;


   protected body Serialise is

      procedure Log (Event : in Event_Type; Message : in String := "") is
         Time_Stamp : Time_Span;
      begin
         Time_Stamp := Clock - Init;
         if Event /= No_Event then
            Ada.Text_IO.Put_Line (Event_Type'Image (Event) & Two_Tabs & Message & Two_Tabs &
                                 Duration'Image (To_Duration (Time_Stamp)));
         else
            Ada.Text_IO.Put_Line (Message);
         end if;
      end Log;

   end Serialise;


   procedure Log (Event : in Event_Type; Message : in String := "") is
   begin
      Serialise.Log (Event, Message);
   end Log;


end Logging_Support;

