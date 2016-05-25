with Rx.Consumer;
with Rx.Root;

generic
   type T (<>) is private;
package Rx.Producer is

   pragma Preelaborate;

   package Downstream is new Rx.Consumer (T);

   subtype Observer is Downstream.Observer;

   type Observable is abstract new Root.Observable with null record;

   procedure Subscribe   (O : in out Observable;
                          S : access Observer'Class) is abstract;

end Rx.Producer;
