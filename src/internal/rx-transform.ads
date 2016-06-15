with Rx.Links;
with Rx.Typed;

with Ada.Finalization; use Ada.Finalization;

generic
   with package From is new Rx.Typed (<>);
   with package Into is new Rx.Typed (<>);
package Rx.Transform is

--     pragma Preelaborate;

   type Func1 is access function (V : From.Type_Traits.T) return Into.Type_Traits.T;

   package Typed is new Rx.Links (From, Into);

   type Teller is new Controlled with null record;
   overriding procedure Finalize (T : in out Teller);

   --  This type is not strictly necessary, but by having it with its own "&" we can disambiguate better
   --  from same-type operators, leading to less prefixing necessary
   type Operator is abstract new Typed.Link with record
      T : Teller;
   end record;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Observable;
                 R : Typed.Link'Class)
                 return Into.Observable renames Typed."&";
--   is (Typed."&" (L, Typed.Link'Class (R))) with Inline;

end Rx.Transform;
