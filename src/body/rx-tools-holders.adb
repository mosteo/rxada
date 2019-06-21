with Ada.Unchecked_Deallocation;

with Gnat.IO; use Gnat.IO;

with Rx.Debug;

package body Rx.Tools.Holders is

   function "+" (I : Indef) return Definite is
   begin
      return (Controlled with Actual => new Indef'(I));
   end "+";

   ----------
   -- Hold --
   ----------

   procedure Hold (D : in out Definite; I : Indef) is
   begin
      if D.Actual /= null then
         D.Finalize;
      end if;
      D.Actual := new Indef'(I);
   end Hold;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (D : in out Definite) is
   begin
      if D.Actual /= null then
         --           Put_Line ("initialize");
         raise Program_Error;
      end if;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (D : in out Definite) is
   begin
      if D.Actual /= null then
         D.Actual := new Indef'(D.Actual.all);
      end if;
   exception
      when others =>
         Put_Line (Id & ": alloc exception (adjust)");
--           Rx.Debug.Print (E);
         raise;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (D : in out Definite) is
      procedure Free is new Ada.Unchecked_Deallocation (Indef, Indef_Access);
   begin
      if D.Actual /= null then
         Free (D.Actual);
      end if;
   exception
      when E : others =>
         Put_Line (Id & ": alloc exception (finalize)");
         Rx.Debug.Print (E);
         raise;
   end Finalize;

   ---------
   -- Ref --
   ---------

   function Ref  (D : in out Definite) return Reference is
   begin
      return Reference'(Actual => D.Actual);
   end Ref;

   ----------
   -- CRef --
   ----------

   function CRef (D : Definite) return Const_Ref is (Actual => D.Actual);

end Rx.Tools.Holders;
