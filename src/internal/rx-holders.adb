with Ada.Unchecked_Deallocation;

package body Rx.Holders is

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
--           Put_Line ("adjust");
         D.Actual := new Indef'(D.Actual.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (D : in out Definite) is
      procedure Free is new Ada.Unchecked_Deallocation (Indef, Indef_Access);
   begin
      if D.Actual /= null then
--           Put_Line ("finalize");
         Free (D.Actual);
      end if;
   end Finalize;

end Rx.Holders;
