generic
   with function Image (V : T) return String is <>;
package Rx.Observables.Image is

   function List_Image (L : T_List) return String;
   --  A default style of "(x, y, z, ...)"

   function Print (With_Timestamp : Boolean := True) return Operator;

   function Print (With_Timestamp : Boolean := True) return List_Preserver;

private

   function Addressable_Image (V : T) return String;
   --  For some reason, Image can be used directly
   --  Additionally, a gnat bug precludes using a expression function for this body

   function Print (With_Timestamp : Boolean := True) return Operator is
     (Observables.Print (Addressable_Image'Access, With_Timestamp));

   package RxPrintList is new Rx.Op.Print (List_Preservers);

   function Print (With_Timestamp : Boolean := True) return List_Preserver is
     (RxPrintList.Create (List_Image'Access, With_Timestamp));

end Rx.Observables.Image;
