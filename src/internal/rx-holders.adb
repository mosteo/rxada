package body Rx.Holders is

   ---------
   -- "+" --
   ---------

   function "+" (I : Indef) return Definite is
   begin
      return List : Definite do
         List.Append (I);
      end return;
   end "+";

   ---------
   -- Ref --
   ---------

   function Ref (I : aliased in out Definite) return Definites.Reference_Type is
   begin
      return I.Reference (I.First);
   end Ref;

   ----------
   -- CRef --
   ----------

   function CRef (I : Definite) return Definites.Constant_Reference_Type is
   begin
      return I.Constant_Reference (I.First);
   end CRef;

end Rx.Holders;
