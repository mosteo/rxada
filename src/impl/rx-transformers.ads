with Rx.Actions.Transform;
with Rx.Errors;
with Rx.Holders;
with Rx.Impl.Links;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transformers is

   pragma Preelaborate;

   --  Shortcuts and also bug workaround
   subtype Observable is From.Observable;
   subtype Observer   is Into.Observer;
   subtype Subscriber is Into.Subscriber;

   subtype Into_Observable is Into.Observable; -- Workarounds a bug

   package Actions is new Rx.Actions.Transform (From.Contracts, Into.Contracts);

   --  Transformative operator scaffolding:

   package Links is new Rx.Impl.Links (From);

   package Child_Holders is new Rx.Holders (Into.Subscriber'Class, "transform.observer'class");
   type Child_Holder is new Child_Holders.Definite with null record;

   --  This type is not strictly necessary, but by having it with its own "&" we can disambiguate better
   --  from same-type operators, leading to less prefixing necessary
   type Transformer is abstract new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with private;

   subtype Operator is Transformer'Class;

   --  To have common code not lost, new operators should extend this one, leaving the original
   --  Consumer interface intact (On_Next, etc). Instead, these versions that receive the Observer
   --  as parameter should be overriden.

   procedure On_Next (This  : in out Transformer;
                      V     :        From.T;
                      Child : in out Into.Observer) is abstract;
   --  Must always be provided

   procedure On_Completed (This  : in out Transformer;
                           Child : in out Into.Observer);
   --  By default calls Child.On_Complete

   procedure On_Error (This  : in out Transformer;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer);
   --  By default calls Child.On_error

   --  FOLLOWING CAN BE OVERRIDEN IF DEFAULT BEHAVIOR HAS TO BE MODIFIED, BUT THESE ARE PROPER DEFAULTS

   overriding
   function Is_Subscribed (This : Transformer) return Boolean;

   overriding
   procedure Subscribe (Producer : in out Transformer;
                        Consumer : in out Into.Subscriber);

   procedure Set_Child (This : in out Transformer; Child : Into.Subscriber);
   -- Can be used to override the default "&" behavior

   not overriding function Get_Child (This : in out Transformer)
                                      return Child_Holders.Reference;

   overriding
   procedure Unsubscribe (This : in out Transformer);
   --  Once the child is no longer needed let it gooo!


   --  DO NOT OVERRIDE THE STANDARD FOLLOWING METHODS IN NEW OPERATORS
   --  OR ELSE CALL THE PARENT IMPLEMENTATIONS

   overriding
   procedure On_Next (This : in out Transformer; V : From.T);
   --  By default calls the explicit On_Next above

   overriding
   procedure On_Completed (This : in out Transformer);
   --  By default calls downstream On_Completed

   overriding
   procedure On_Error (This : in out Transformer; Error : in out Errors.Occurrence);
   --  By default calls downstream On_Error

   function Will_Observe (Producer : From.Observable;
                          Consumer : Transformer'Class)
                          return Into.Observable;
   --  This does the magic of preparing a passive chain, ready for actual subscription/observation

   function "&" (Producer : From.Observable;
                 Consumer : Transformer'Class)
                 return Into.Observable renames Will_Observe;

private

   type Transformer is abstract new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with record
      Child : Child_Holder;
   end record;

   not overriding function Has_Child (This : Transformer) return Boolean is (not This.Child.Is_Empty);

   not overriding function Get_Child (This : in out Transformer)
                                      return Child_Holders.Reference is (This.Child.Ref);

   overriding function Is_Subscribed (This : Transformer) return Boolean is (not This.Child.Is_Empty);

end Rx.Transformers;
