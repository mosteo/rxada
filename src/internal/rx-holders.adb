with Ada.Unchecked_Deallocation;

with Gnat.IO; use Gnat.IO;

package body Rx.Holders is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (D : in out Definite) is
   begin
      if D.Actual /= null then
         Put_Line ("adjust");
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
         Put_Line ("finalize");
      end if;
      Free (D.Actual);
   end Finalize;

end Rx.Holders;
