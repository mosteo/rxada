with Ada.Text_IO; use Ada.Text_IO;

with Rx;

procedure Rx.Examples.Basic is

   procedure Put_Line (I : Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;   

   --  O : RxStrings.Observable'Class := RxStrings.Just ("");

begin
   RxStrings.Just ("").Subscribe (Ada.Text_IO.Put_Line'Access);
end Rx.Examples.Basic;
