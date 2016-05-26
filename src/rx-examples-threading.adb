with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image;

with Rx.Debug;

procedure Rx.Examples.Threading is

   procedure Julai (I : Integer) is
      use System;
      X : Integer;
   begin
      X := I;
      delay 1.0;
      Put_Line (X'Img);
      Put_Line (System.Address_Image (X'Address));
   end Julai;

   generic
      Val : Integer;
      with procedure Blah (I : Integer);
   package Z is
      task Go;
   end Z;

   package body Z is
      task body Go is
      begin
         loop
            Blah (Val);
         end loop;
      end Go;
   end Z;

   package Z1 is new Z (1, Julai);
   package Z2 is new Z (2, Julai);

begin
   -- Test;
   Put_Line ("After main");
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Threading;
