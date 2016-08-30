private with Rx.Shared_Data;

package Rx.Subscriptions is

   pragma Preelaborate;

   pragma Compile_Time_Warning (True,
                                "To complete subscription implementation, I have to add the subscription to " &
                                  "producers, consumers and links");

   type Subscription (<>) is tagged private;

   function Subscribe return Subscription;

   procedure Unsubscribe (S : in out Subscription);

   function Is_Subscribed (S : in out Subscription) return Boolean;

   type No_Subscription is null record;
   -- For when we do not care at all

private

   type Boolean_Access is access Boolean;

   --  This probably can be done more lightweight since it involves a single boolean check
   package Shared_Booleans is new Rx.Shared_Data (Boolean, Boolean_Access);

   type Subscription is new Shared_Booleans.Proxy with null record;

end Rx.Subscriptions;
