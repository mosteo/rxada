with Ada.Exceptions;

with Rx.Contracts;
with Rx.Errors;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Defaults with Preelaborate is

   --  Defaults to be used elsewhere

   procedure Default_Error_Handler (This   : in out Contracts.Observer'Class;
                                    Except :        Ada.Exceptions.Exception_Occurrence);
   --  Calls down On_Error for non-critical exceptions, otherwise raises
   --  If no downstream, just report

   procedure Default_On_Error (E : Errors.Occurrence);
   --  Dump error to console and re-raise

   type Observer is new Contracts.Observer with null record;
   --  Does nothing but properly reporting in On_Error

   overriding procedure On_Next (This : in out Observer; V : Contracts.T) is null;

   overriding procedure On_Complete  (This : in out Observer) is null;

   overriding procedure On_Error (This : in out Observer;
                                  E    :        Errors.Occurrence);

end Rx.Defaults;
