with Rx.Interfaces;

generic
   type T (<>) is private;
   with function Image (V : T) return String is <>;
package Rx.Typed_Values is

   type Typed_Value is new Rx.Interfaces.Value with null record;

   overriding
   function Image (V : Typed_Value) return String is ("Unimplemented");

   function Unwrap (V : Typed_Value) return T;

   function Wrap (V : T) return Typed_Value'Class;

end Rx.Typed_Values;
