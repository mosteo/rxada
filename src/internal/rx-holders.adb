with Ada.Unchecked_Deallocation;

package body Rx.Holders is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (D : in out Definite) is
   begin
      if D.Actual /= null then
         D.Actual := new Indef'(D.Actual.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (D : in out Definite) is
      procedure Free is new Ada.Unchecked_Deallocation (Indef, Indef_Access);
   begin
      Free (D.Actual);
   end Finalize;

end Rx.Holders;
