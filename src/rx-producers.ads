with Rx.Consumers;
with Rx.Holders;

generic
   type T (<>) is private;
package Rx.Producers is

   pragma Preelaborate;

   package Consumers is new Rx.Consumers (T);

   type Observable is interface;

   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Consumers.Observer'Class) is abstract;

   package Holders is new Rx.Holders (Observable'Class);
   type Holder is new Holders.Definite with null record;


   type Subscriber is interface;

   procedure Set_Parent (This : in out Subscriber; Parent : Observable'Class) is abstract;
   function  Get_Parent (This :        Subscriber) return Observable'Class is abstract;

   -- Convenience type since we'll need all observers to be also subscribers
   type Subscriptor is abstract new Consumers.Observer and Subscriber with private;

   overriding
   procedure Set_Parent (This : in out Subscriptor; Parent : Observable'Class);
   overriding
   function  Get_Parent (This :        Subscriptor) return Observable'Class;

private

   type Subscriptor is abstract new Consumers.Observer and Subscriber with record
      Parent : Holder;
   end record;

end Rx.Producers;
