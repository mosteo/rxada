with Rx.Holders;

generic
   type T (<>) is private;
package Rx.Consumers is

   pragma Preelaborate;

   type Observer is interface;
   procedure OnNext      (This : in out Observer; V : T) is abstract;
   procedure OnCompleted (This :        Observer) is null;

   package Holders is new Rx.Holders (Observer'Class);
   type Holder is new Holders.Definite with null record;

end Rx.Consumers;
