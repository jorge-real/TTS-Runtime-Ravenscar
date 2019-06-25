package body Epoch_Support is

   Init_Time : constant Time := Clock + Milliseconds (1000);


   -----------
   -- Epoch --
   -----------

   function Epoch return Time is (Init_Time);

end Epoch_Support;
