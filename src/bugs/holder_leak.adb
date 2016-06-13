with Ada.Containers.Indefinite_Holders;
with Ada.Text_IO; use Ada.Text_IO;

procedure Holder_Leak is
   use Ada.Containers;
   type Indef is array (Integer range <>) of Integer;
   package Holders is new Indefinite_Holders (Indef);
   type Def is new Holders.Holder with null record;

   -- This wrapper type is necessary for the leak to manifest
   type Outer is record
      Inner : Def;
   end record;

   procedure Eat (O : Outer) is
   begin
      for I of O.Inner.Constant_Reference loop
      -- Using .Element instead there is no leak
         Put_Line (I'Img);
      end loop;
   end Eat;

begin
   for I in 1 .. 666 loop
      Eat ((Inner => To_Holder ((1, 2, 3, 4, 5))));
   end loop;
end Holder_Leak;
