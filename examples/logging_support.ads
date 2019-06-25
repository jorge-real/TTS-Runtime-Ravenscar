
package Logging_Support is
------------------------------------------------------------------------
--  | Package Giving Operations Useful for logging tasks activity
--  | WITH-ed by the body of a package to provide an easy way to
--  | trace events related to task activation and completion
--  | Implemented as a modification to the Debugging_Support package
--  | from Michael B. Feldman, The George Washington University
--  | Author: Jorge Real
--  | Last Modified: October 1998
------------------------------------------------------------------------

--     type Switch is (Off, On);
   type Event_Type is (No_Event, Start_Task, Stop_Task, Mode_Change,
                       Start_Resource, Stop_Resource, Missed_Deadline);

--     procedure Set_Log(Which_Way: in Switch; File_Name: String := "");
--     -- Pre:  WhichWay is defined
--     -- Post: Logging support is turned On or Off, as the case may be;
--     --       If FileName = "", logging output goes to Standard_Output;
--     --       otherwise, debugging output goes to the given file.

   procedure Log (Event : in Event_Type; Message : in String := "");
   --  Pre:  Event is defined
   --  Post: Writes a message to Standard_Output or an external file
   --       that registers the activation of the specified event

end Logging_Support;
