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

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Producers.Observable'Class;
                 R : Operator'Class)
                 return Into.Producers.Observable'Class
   is (Typed."&" (L, Typed.Link'Class (R)))
   with Inline;

end Rx.Transform;
