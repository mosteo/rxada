with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.From is

   generic
      type Index_Type is (<>);
   package From_Array is

      type Array_Type is array (Index_Type range <>) of Typed.Type_Traits.D;

      function From (A : Array_Type) return Typed.Producers.Observable'Class;

   end From_Array;

end Rx.From;
