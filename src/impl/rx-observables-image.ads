generic
   with function Image (V : T) return String is <>;
package Rx.Observables.Image is

   function List_Image (L : T_List) return String;
   --  A default style of "(x, y, z, ...)"

   function Print (With_Timestamp : Boolean := True) return Operator;

   function Print_List (With_Timestamp : Boolean := True) return T_To_List.Operator'Class;

private

   function Addressable_Image (V : T) return String is (Image (V));

   function Print (With_Timestamp : Boolean := True) return Operator is
     (Observables.Print (Addressable_Image'Access, With_Timestamp));

end Rx.Observables.Image;
