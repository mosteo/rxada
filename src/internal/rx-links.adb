with Gnat.IO; use Gnat.IO;

package body Rx.Links is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Link;
      Consumer : in out Into.Observer)
   is
      use type Into.Consumers.Holder;
   begin
      if Producer.Has_Parent then
         declare
            Parent : From.Observable := Producer.Get_Parent; -- Our own copy
         begin
            if Consumer in Into.Producers.Subscriptor'Class then -- Ouch
               Producer.Share (Into.Producers.Subscriptor'Class (Consumer).Subscription);
            end if;
            Producer.Set_Child (Consumer); -- With its own child
            Parent.Subscribe (Producer);
         end;
      else
         raise Constraint_Error with "Attempting subscription without source observable";
      end if;
   end Subscribe;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (This : in out Link) return Into.Consumers.Holders.Reference is
   begin
      return This.Child.Ref;
   end Get_Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (This : in out Link; Child : Into.Observer) is
      use type Into.Consumers.Holder;
   begin
      This.Child.Hold (Child);
   end Set_Child;

   -------------------
   -- Release_Child --
   -------------------

   procedure Release_Child (This : in out Link) is
   begin
      This.Child.Clear;
   end Release_Child;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Observable;
                 R : Link'Class)
                 return Into.Observable
   is
   begin
      return Actual : Link'Class := R do
         Actual.Set_Parent (L);
      end return;
   end "&";

end Rx.Links;
