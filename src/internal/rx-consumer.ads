with Ada.Exceptions;

generic
   type T (<>) is private;
package Rx.Consumer is

   type Observer is limited interface;

   procedure OnNext      (This  : in out Observer; V : T) is abstract;

   procedure OnCompleted (This  : in out Observer) is null;

   procedure OnError     (This  : in out Observer;
                          Error : 	 Ada.Exceptions.Exception_Occurrence) is null;

end Rx.Consumer;
