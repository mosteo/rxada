-- with Gnat.IO; use Gnat.IO;

package body Rx.Producers is

   ----------------
   -- Set_Parent --
   ----------------

   overriding procedure Set_Parent
     (This : in out Subscriptor;
      Parent : Observable'Class)
   is
   begin
      This.Parent.Hold (Parent);
   end Set_Parent;

end Rx.Producers;
