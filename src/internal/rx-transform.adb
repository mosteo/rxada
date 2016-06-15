with Gnat.IO; use Gnat.IO;

package body Rx.Transform is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (T : in out Teller) is
   begin
      Put_Line ("op finalize ");
   end Finalize;

end Rx.Transform;
