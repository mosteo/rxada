package body Rx.Holders is

   ---------
   -- "+" --
   ---------

   function "+" (I : Indef) return Definite is
      D : Definite;
   begin
      D.Append (I);
      return D;
   end "+";

   -------------
   -- Element --
   -------------

   function Element (I : Definite) return Indef is
   begin
      return I.First_Element;
   end Element;

end Rx.Holders;
