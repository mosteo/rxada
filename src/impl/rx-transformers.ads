with Rx.Actions.Transform;
with Rx.Errors;
with Rx.Holders;
with Rx.Impl.Links;
with Rx.Impl.Operators;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transformers with Preelaborate is

   --  Renamings for bug workarounds
   subtype From_Observable is From.Observable'Class;
   subtype Into_Observable is Into.Observable'Class;

   package Actions is new Rx.Actions.Transform (From.Contracts, Into.Contracts);

   --  Transformative operator scaffolding:
   package Links is new Rx.Impl.Links (From);

   type Operator is new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with private;
   --  This is the fundamental type that bridges observables y observers doing something along the way
   --  For simpler operator creation, override a Impl.Operators.Operator, and wrap it here with Create
   --  A Transformer wraps an operator taking care of checks that are common to most operators

   package Implementation is new Rx.Impl.Operators (From, Into);

   ------------
   -- Create --
   ------------

   function Create (Using : Implementation.Operator'Class) return Operator'Class;

   --  There should be no need to override the following methods

   function Will_Observe (Producer : From.Observable;
                          Consumer : Operator'Class)
                          return Into.Observable;
   --  This does the magic of preparing a passive chain, ready for actual subscription/observation

   function "&" (Producer : From.Observable;
                 Consumer : Operator'Class)
                 return Into.Observable renames Will_Observe;

   --  Other overridings that can be left as-is

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer :        Into.Subscriber);

   overriding
   procedure Unsubscribe (This : in out Operator);
   --  Clear subscriber

   overriding
   procedure On_Next (This : in out Operator; V : From.T);
   --  By default calls the explicit On_Next above

   overriding
   procedure On_Completed (This : in out Operator);
   --  By default calls downstream On_Completed

   overriding
   procedure On_Error (This : in out Operator; Error : Errors.Occurrence);
   --  By default calls downstream On_Error

   overriding
   function Is_Subscribed (This : Operator) return Boolean;

private

   package Holders is new Rx.Holders (Implementation.Operator'Class);

   type Operator is new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Subscriber
   with record
      Actual : Holders.Definite;
   end record;

   overriding function Is_Subscribed (This : Operator) return Boolean is
     (This.Actual.Is_Valid and then This.Actual.CRef.Is_Subscribed);

   function Create (Using : Implementation.Operator'Class) return Operator'Class is
     (Operator'(Links.Downstream with
                Actual => Holders.Hold (Using)));

   function Get_Operator (This : in out Operator'Class) return Holders.Reference is
     (This.Actual.Ref);

end Rx.Transformers;
