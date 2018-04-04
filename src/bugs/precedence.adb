procedure Precedence is

   generic
   package Nested is

      type Object is tagged null record;

      procedure Method (O : Object) is null;

   end Nested;

   package Nest is new Nested;

   function Op (X, Y : Integer) return Nest.Object'Class is
      pragma Unreferenced (X, Y);
   begin
      return Nest.Object'(null record);
   end Op;

   function "&" (X, Y : Integer) return Nest.Object'Class renames Op;

   N : constant Nest.Object'Class := Op (1, 2);
   M : constant Nest.Object'Class := 1 & 2;

begin
   N.Method; -- Of course this works
   M.Method;

   Op (1, 2).Method;  --  Fine
   "&" (1, 2).Method;  --  Fine too
   --(1 & 2).Method;  --  Error: statement expected (gpl2015/16)
end Precedence;
