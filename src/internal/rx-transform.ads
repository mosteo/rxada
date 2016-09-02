with Rx.Errors;
with Rx.Links;
with Rx.Typed;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

--     pragma Preelaborate;

   type Func1 is access function (V : From.Type_Traits.T) return Into.Type_Traits.T;

   package Typed is new Rx.Links (From, Into);

   --  This type is not strictly necessary, but by having it with its own "&" we can disambiguate better
   --  from same-type operators, leading to less prefixing necessary
   type Operator is abstract new Typed.Link with null record;

   --  To have common code not lost, new operators should extend this one, leaving the original
   --  Consumer interface intact (On_Next, etc). Instead, these versions that receive the Observer
   --  as parameter should be overriden.

   procedure On_Next (This  : in out Operator;
                      V     :        From.T;
                      Child : in out Into.Observer'Class) is abstract;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Observable;
                 R : Operator'Class)
                 return Into.Observable
   is (Typed."&" (L, Typed.Link'Class (R)))
   with Inline;

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
