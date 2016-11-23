with Rx.Actions.Transform;
with Rx.Errors;
with Rx.Holders;
with Rx.Impl.Links;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   pragma Preelaborate;

   package Actions is new Rx.Actions.Transform (From.Contracts, Into.Contracts);

   --  Transformative operator scaffolding:

   package Links is new Rx.Impl.Links (From);

   --  This type is not strictly necessary, but by having it with its own "&" we can disambiguate better
   --  from same-type operators, leading to less prefixing necessary
   type Operator is abstract new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with private;

   subtype Transformer is Operator'Class;

   --  To have common code not lost, new operators should extend this one, leaving the original
   --  Consumer interface intact (On_Next, etc). Instead, these versions that receive the Observer
   --  as parameter should be overriden.

   procedure On_Next (This  : in out Operator;
                      V     :        From.Type_Traits.T;
                      Child : in out Into.Observer'Class) is abstract;
   --  Must always be provided

   procedure On_Completed (This : in out Operator;
                           Child : in out Into.Observer'Class);
   --  By default calls Child.On_Complete

   procedure On_Error (This  : in out Operator;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer'Class);
   --  By default calls Child.On_error

   --  FOLLOWING CAN BE OVERRIDEN IF DEFAULT BEHAVIOR HAS TO BE MODIFIED, BUT THESE ARE PROPER DEFAULTS

   overriding
   function Is_Subscribed (This : in out Operator) return Boolean;

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Subscriber);

   procedure Set_Child (This : in out Operator; Child : Into.Subscriber);
   -- Can be used to override the default "&" behavior

   overriding
   procedure Unsubscribe (This : in out Operator);
   --  Once the child is no longer needed let it gooo!


   --  DO NOT OVERRIDE THE STANDARD FOLLOWING METHODS IN NEW OPERATORS
   --  OR ELSE CALL THE PARENT IMPLEMENTATIONS

   overriding
   procedure On_Next (This : in out Operator; V : From.T);
   --  By default calls the explicit On_Next above

   overriding
   procedure On_Completed (This : in out Operator);
   --  By default calls downstream On_Completed

   overriding
   procedure On_Error (This : in out Operator; Error : in out Errors.Occurrence);
   --  By default calls downstream On_Error

   function Will_Observe (Producer : From.Observable;
                          Consumer : Operator'Class)
                          return Into.Observable;
   --  This does the magic of preparing a passive chain, ready for actual subscription/observation

   function "&" (Producer : From.Observable;
                 Consumer : Operator'Class)
                 return Into.Observable renames Will_Observe;

private

   package Child_Holders is new Rx.Holders (Into.Subscriber'Class, "transform.observer'class");
   type Child_Holder is new Child_Holders.Definite with null record;

   type Operator is abstract new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with record
      Child : Child_Holder;
   end record;

   function Has_Child (This : Operator) return Boolean is (not This.Child.Is_Empty);

   function Get_Child (This : in out Operator) return Child_Holders.Reference is (This.Child.Ref);

   overriding
   function Is_Subscribed (This : in out Operator) return Boolean is (not This.Child.Is_Empty);

end Rx.Transform;
