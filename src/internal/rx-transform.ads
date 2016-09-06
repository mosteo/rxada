with Rx.Contracts;
with Rx.Errors;
with Rx.Holders;
with Rx.Impl.Links;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   pragma Preelaborate;

   type Func1 is access function (V : From.Type_Traits.T) return Into.Type_Traits.T;

   package Links is new Rx.Impl.Links (From);

   --  This type is not strictly necessary, but by having it with its own "&" we can disambiguate better
   --  from same-type operators, leading to less prefixing necessary
   type Operator is abstract new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Observer and
     Rx.Contracts.Subscriber
       with private;

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

   --  DO NOT OVERRIDE THE STANDARD FOLLOWING METHODS IN NEW OPERATORS
   --  OR ELSE CALL THE PARENT IMPLEMENTATIONS

   overriding
   function Is_Subscribed (This : Operator) return Boolean;

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

private

   package Child_Holders is new Rx.Holders (Into.Observer'Class, "transform.observer'class");
   type Child_Holder is new Child_Holders.Definite with null record;

   type Operator is abstract new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Observer and
     Rx.Contracts.Subscriber
   with record
      Child : Child_Holder;
   end record;

   procedure Set_Child (This : in out Operator; Child : Into.Observer);
   -- Can be used to override the default "&" behavior

   function Has_Child (This : Operator) return Boolean is (not This.Child.Is_Empty);

   function Get_Child (This : in out Operator) return Child_Holders.Reference is (This.Child.Ref);

   procedure Release_Child (This : in out Operator);
   --  Once the child is no longer needed let it gooo!

   overriding
   procedure Observe (Producer : in out Operator;
                      Consumer : in out Into.Observer);

   overriding
   function Is_Subscribed (This : Operator) return Boolean is (not This.Child.Is_Empty);

end Rx.Transform;
