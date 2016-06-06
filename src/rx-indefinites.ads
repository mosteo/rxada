with Rx.Sources;
with Rx.Subscriptions;
with Rx.Typed;

-- Entry point for a user to declare a new type to be used in Rx shenanigans
generic
   type T (<>) is private;
package Rx.Indefinites is

   package Typed   is new Rx.Typed (T);
   package Sources is new Rx.Sources (Typed);

   --  Make visible the "&" here so only this unit has to be use'd
   function "&" (L : Typed.Producers.Observable'Class;
                 R : Typed.Consumers.Observer'Class) return Subscriptions.Subscription renames Sources."&";

end Rx.Indefinites;
