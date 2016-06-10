with Rx.Typed;

pragma Warnings (Off);
generic
   with package Typed is new Rx.Typed (<>);

   type Container is limited private;
   type Cursor is private;
   with function First (C : Container) return Cursor is <>;
   with function Next (C : Cursor) return Cursor is <>;
   with function Element (C : Cursor) return Typed.Type_Traits.T is <>;
   with function Has_Element (C : Cursor) return Boolean is <>;
package Rx.Traits.Iterable is

   pragma Preelaborate;

end Rx.Traits.Iterable;
