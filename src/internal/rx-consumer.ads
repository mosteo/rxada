generic
   type T (<>) is private;
package Rx.Consumer is

   pragma Preelaborate;

   type Observer is interface;

   procedure OnNext      (This : in out Observer; V : T) is abstract;

   procedure OnCompleted (This : in out Observer) is null;

end Rx.Consumer;
