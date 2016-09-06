with Rx.Contracts;
with Rx.Errors;
-- with Rx.Links;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

   pragma Preelaborate;

   type Func1 is access function (V : From.Type_Traits.T) return Into.Type_Traits.T;

--   package Typed is new Rx.Links (From, Into);

   --  This type is not strictly necessary, but by having it with its own "&" we can disambiguate better
   --  from same-type operators, leading to less prefixing necessary
   type Operator is abstract new
     Into.Contracts.Observable and
     From.Contracts.Observer and
     Rx.Contracts.Subscriber
       with null record;

   --  To have common code not lost, new operators should extend this one, leaving the original
   --  Consumer interface intact (On_Next, etc). Instead, these versions that receive the Observer
   --  as parameter should be overriden.

   procedure On_Next (This  : in out Operator;
                      V     :        From.Type_Traits.T;
                      Child : in out Into.Consumers.Observer'Class) is abstract;
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
   procedure On_Next (This : in out Operator; V : From.T);
   --  By default calls the explicit On_Next above

   overriding
   procedure On_Completed (This : in out Operator);
   --  By default calls downstream On_Completed

   overriding
   procedure On_Error (This : in out Operator; Error : in out Errors.Occurrence);
   --  By default calls downstream On_Error

end Rx.Transform;
