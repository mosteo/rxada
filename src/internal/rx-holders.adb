with Ada.Unchecked_Deallocation;

with Gnat.IO; use Gnat.IO;

package body Rx.Holders is

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
