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

   type Transformer (<>) is new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with private;
   --  This is the fundamental type that bridges observables y observers doing something along the way
   --  For simpler operator creation, see the Operator type below
   --  A Transformer wraps an operator taking care of checks that are common to most operators

   type New_Operator is abstract new From.Contracts.Subscriber with private;
   --  This is the type recommended to override for implementing new operators
   --  In conjunction with the Create function here, it provides a proper transformer
   --  with correct management of subscriptions and errors, and proper defaults

   ------------
   -- Create --
   ------------

   function Create (Using : New_Operator'Class) return Transformer'Class;


   --  Defaults to be overriden per operator

   overriding procedure On_Next (This : in out New_Operator; V : From.T) is null;
   --  Just drops the value

   overriding procedure On_Completed (This : in out New_Operator);
   --  Just passes it along

   overriding procedure On_Error (This : in out New_Operator; Error : Errors.Occurrence);
   --  Just passes it along

   not overriding procedure Set_Subscriber (This : in out New_Operator; Observer : Into.Subscriber'Class);
   --  Override only if you need a modified observer (for example a Shared one)


   --  From this point on there should be no need to override the rest of methods

   function Get_Subscriber (This : in out New_Operator'Class) return Into.Holders.Subscribers.Reference;
   --  Use this in order to get the next observer in chain

   overriding function Is_Subscribed (This : New_Operator) return Boolean;
   --  Proper default, no need to override

   overriding procedure Unsubscribe (This : in out New_Operator);
   --  Proper default, no need to override

   --  There should be no need to override the following methods

   function Will_Observe (Producer : From.Observable;
                          Consumer : Transformer'Class)
                          return Into.Observable;
   --  This does the magic of preparing a passive chain, ready for actual subscription/observation

   function "&" (Producer : From.Observable;
                 Consumer : Transformer'Class)
                 return Into.Observable renames Will_Observe;

   --  Other overridings that can be left as-is

   overriding
   procedure Subscribe (Producer : in out Transformer;
                        Consumer : in out Into.Subscriber);

   overriding
   procedure Unsubscribe (This : in out Transformer);

   overriding
   procedure On_Next (This : in out Transformer; V : From.T);
   --  By default calls the explicit On_Next above

   overriding
   procedure On_Completed (This : in out Transformer);
   --  By default calls downstream On_Completed

   overriding
   procedure On_Error (This : in out Transformer; Error : Errors.Occurrence);
   --  By default calls downstream On_Error

   overriding
   function Is_Subscribed (This : Transformer) return Boolean;

private

   type New_Operator is abstract new From.Contracts.Subscriber with record
      Subscriber : Into.Holders.Subscriber;
   end record;

   package Operator_Holders is new Rx.Holders (New_Operator'Class);

   type Transformer is new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with record
      Operator : Operator_Holders.Definite;
   end record;

   overriding function Is_Subscribed (This : Transformer) return Boolean is
     (This.Operator.Is_Valid and then This.Operator.CRef.Is_Subscribed);

   overriding function Is_Subscribed (This : New_Operator) return Boolean is (This.Subscriber.Is_Valid);

   function Create (Using : New_Operator'Class) return Transformer'Class is
     (Transformer'(Links.Downstream with
                   Operator => Operator_Holders.Hold (Using)));

   function Get_Subscriber (This : in out New_Operator'Class) return Into.Holders.Subscribers.Reference is
     (This.Subscriber.Ref);

   function Get_Operator (This : in out Transformer'Class) return Operator_Holders.Reference is
      (This.Operator.Ref);

   procedure Clear (This : in out Transformer'Class);
   --  Dispose of as much as possible

end Rx.Transformers;
