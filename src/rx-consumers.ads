with Rx.Holders;
with Rx.Values;

package Rx.Consumers is

   pragma Preelaborate;

   type Observer is interface;
   procedure OnNext      (This : Observer; V : Values.Value'Class) is abstract;
   procedure OnCompleted (This : Observer) is null;

   package Holders is new Rx.Holders (Observer'Class);
   type Holder is new Holders.Definite with null record;

end Rx.Consumers;
