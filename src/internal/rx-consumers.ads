with Ada.Exceptions;

with Rx.Errors;
with Rx.Holders;

generic
   type T (<>) is private;
package Rx.Consumers is

   pragma Preelaborate;

   type Observer is interface;
   procedure On_Next      (This : in out Observer; V : T) is abstract;
   procedure On_Completed (This : in out Observer) is null;
   procedure On_Error     (This : in out Observer; Error : Errors.Occurrence) is null;

   package Holders is new Rx.Holders (Observer'Class);
   type Holder is new Holders.Definite with null record;

   procedure Default_Error_Handler (This : in out Observer'Class; Except : Ada.Exceptions.Exception_Occurrence);

end Rx.Consumers;
