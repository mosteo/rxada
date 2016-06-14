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
            Parent : From.Observable := Producer.Get_Parent;
         begin
            Producer.Set_Child (Consumer);
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

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Link) is
   begin
      This.Child.Ref.On_Completed;
      This.Child.Clear; -- Not strictly necessary, but frees memory somewhat earlier
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Link; Error : in out Errors.Occurrence) is
   begin
      This.Child.Ref.On_Error (Error); -- Pass it down
      This.Child.Clear; -- Not strictly necessary, but frees memory somewhat earlier
   end On_Error;

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
