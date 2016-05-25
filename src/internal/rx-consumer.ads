with Ada.Exceptions;

with Rx.Root;

generic
   type T (<>) is private;
package Rx.Consumer is

   pragma Preelaborate;

   type Observer is abstract new Rx.Root.Observer with null record;

   procedure OnNext      (This : in out Observer; V : T) is null;
   procedure OnCompleted (This : in out Observer) is null;

end Rx.Consumer;
