private with Ada.Containers.Indefinite_Holders;

generic
   type T (<>) is private;
package Rx.Values.Typed is

   type Value is new Values.Value with private;

   function Unwrap (V : Value) return T;

   function Wrap (V : T) return Value'Class;

private

   package H is new Ada.Containers.Indefinite_Holders (T);

   type Value is new Values.Value with record
      V : H.Holder;
   end record;

   function Unwrap (V : Value) return T is (V.V.Element);

   function Wrap (V : T) return Value'Class is (Value'(V => H.To_Holder (V)));

end Rx.Values.Typed;
