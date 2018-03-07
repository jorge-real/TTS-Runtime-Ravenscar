
with TTS_Example_A;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;    use Ada.Text_IO;

procedure Main5 is
begin
   TTS_Example_A.Main;
   delay until Ada.Real_Time.Time_Last;
exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Main5;
