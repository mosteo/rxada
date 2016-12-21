procedure B000 is

   generic
   package P0 is
   end P0;

   generic
      with package AP0 is new P0 (<>);
   package P1 is

      type T (<>) is tagged private;

   private

      type T is tagged null record;

   end P1;

   generic
      with package AP0 is new P0 (<>);
   package P3 is

      package AP1 is new P1 (AP0);

      subtype TT is AP1.T;

   end P3;

   generic
      with package AP3 is new P3 (<>);
   package P2 is

      function Create return AP3.TT'Class;

   end P2;

   package body P2 is

      type T is new AP3.TT with null record;

      function Create return AP3.TT'Class is
      begin
         return T'(AP3.TT with null record);
      end Create;

   end P2;

begin
   null;
end B000;
