
with TTS_Example_C;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;    use Ada.Text_IO;
with System;

procedure Main_C
  with Priority => System.Priority'First
is
begin
   TTS_Example_C.Main;
   delay until Ada.Real_Time.Time_Last;
exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Main_C;
