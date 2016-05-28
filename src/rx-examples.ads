with Rx.Knot;

package Rx.Examples is

   package RxStrings  is new Rx.Knot (String); 
   package RxIntegers is new Rx.Knot (Integer);
   
   package RxStringsToIntegers is new RxStrings.Operators (RxIntegers);

end Rx.Examples;
