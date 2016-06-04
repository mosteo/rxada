with Rx.Holders;
with Rx.Observable;

generic
   type T (<>) is private;
package Rx.Definite_Observable is

   package Definite_Type       is new Rx.Holders (T);
   package Definite_Observable is new Rx.Observable (Definite_Type.Definite);

end Rx.Definite_Observable;
