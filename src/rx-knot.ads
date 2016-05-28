with Rx.Producer;

generic
   type T (<>) is private;
package Rx.Knot is

   type Observable is new Rx.Producer.Observable with private;

   function Just (Val : T) return Observable'Class;

end Rx.Knot;
