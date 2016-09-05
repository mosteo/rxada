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

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe     (This : in out Subscriptor) is
   begin
      if not This.Subscription.Is_Subscribed then
         This.Subscription := Subscriptions.Subscribe;
      end if;
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe   (This : in out Subscriptor) is
   begin
      This.Subscription.Unsubscribe;
   end Unsubscribe;

   -----------
   -- Share --
   -----------

   overriding procedure Share (This : in out Subscriptor; S : Subscriptions.Subscription) is
   begin
      This.Subscription := S;
   end Share;

end Rx.Producers;
