with Rx.Errors;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Links is

   pragma Preelaborate;

--  A type that knows how to build a chain and then how to trigger subscriptions.
--  Used as root for both Transformers and Mutators (the two kinds of Links)
--  Transformers change types, Mutators do not.

   type Link is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
   with private;

   function "&" (L : From.Producers.Observable'Class;
                 R : Link'Class)
                 return Into.Producers.Observable'Class;

   not overriding
   procedure On_Next (This  : in out Link;
                      Child : in out Into.Consumers.Observer'Class;
                      V     : From.Type_Traits.T) is abstract;

   not overriding
   procedure On_Completed (This : in out Link; Child : in out Into.Consumers.Observer'Class) is null;

   overriding
   procedure Subscribe (Producer : in out Link;
                        Consumer : in out Into.Consumers.Observer'Class);

   overriding
   procedure On_Next (This : in out Link; V : From.Type_Traits.T);

   overriding
   procedure On_Completed (This : in out Link);

   overriding
   procedure On_Error (This : in out Link; Error : in out Errors.Occurrence);

private

   type Link is abstract new
     From.Producers.Subscriptor and
     Into.Producers.Observable
       with record
      Child : Into.Consumers.Holder;
   end record;

end Rx.Links;
