with Ada.Real_Time; use Ada.Real_Time;

package Epoch_Support is

   function Epoch return Time
     with Inline;

end Epoch_Support;
