with Rx.Subscribers;

package Rx.Producers is

   pragma Pure;

   type Observable is interface;
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Subscribers.Observer'Class) is abstract;

   subtype Observer is Subscribers.Observer'Class;

end Rx.Producers;
