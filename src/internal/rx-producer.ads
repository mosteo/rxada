with Rx.Consumer;

generic
   type T (<>) is private;
package Rx.Producer is

   package Downstream is new Rx.Consumer (T);

   subtype Observer is Downstream.Observer;

   type Observable is limited interface;

   procedure Subscribe   (O : in out Observable;
                          S : access Observer'Class) is abstract;

end Rx.Producer;
