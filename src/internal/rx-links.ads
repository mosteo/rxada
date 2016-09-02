with Rx.Errors;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Links is

--     pragma Preelaborate;

--  A type that knows how to build a chain and then how to trigger subscriptions.
--  Used as root for both Transform and Operate (the two kinds of Links)
--  Transformers change types, Operaters do not.

   type Link is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
   with private;

   function "&" (L : From.Observable;
                 R : Link'Class)
                 return Into.Observable;

   procedure Set_Child (This : in out Link; Child : Into.Observer);
   -- Can be used to override the default "&" behavior

   function Get_Child (This : in out Link) return Into.Consumers.Holders.Reference;
   -- Can be used within the Observer actions to pass the values along

   procedure Release_Child (This : in out Link);
   --  Once the child is no longer needed let it gooo!

   overriding
   procedure Subscribe (Producer : in out Link;
                        Consumer : in out Into.Observer);

private

   type Link is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
       with record
      Child : Into.Consumers.Holder;
   end record;

end Rx.Links;
