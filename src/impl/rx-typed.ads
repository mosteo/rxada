with Ada.Exceptions;

with Rx.Actions;
with Rx.Contracts;
with Rx.Impl.Definite_Observable;
with Rx.Traits.Types;

generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Typed is

   pragma Preelaborate;

   package Contracts is new Rx.Contracts (Type_Traits.T);
   --  The beginning of it all

   package Actions   is new Rx.Actions.Typed (Type_Traits.T);

   -- Shortcuts
   subtype T is Type_Traits.T;
   subtype D is Type_Traits.D;
   subtype Observable is Contracts.Observable'Class;
   subtype Observer   is Contracts.Observer'Class;
   subtype Subscriber is Contracts.Subscriber'Class;

   procedure Default_Error_Handler (This   : in out Observer'Class;
                                    Except : Ada.Exceptions.Exception_Occurrence);

   package Defobs is new Impl.Definite_Observable (Contracts);

end Rx.Typed;
