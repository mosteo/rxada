with Ada.Exceptions;

with Rx.Errors;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Impl.Events is

   type Kinds is (On_Next, On_Completed, On_Error, Unsubscribe);

   type Event (Kind : Kinds) is private;

   function On_Next (V : Typed.T) return Event;

   function On_Completed return Event;

   function On_Error (E : Errors.Occurrence) return Event;

   function On_Error (E : Ada.Exceptions.Exception_Occurrence) return Event;

   function Unsubscribe return Event;

   function Value (E : Event) return Typed.T
     with Pre => E.Kind = On_Next;

   function Error (E : Event) return Errors.Occurrence
     with Pre => E.Kind = On_Error;

private

   type Event (Kind : Kinds) is record
      case Kind is
         when On_Next      => V : Typed.D;
         when On_Error     => E : Errors.Occurrence;
         when On_Completed => null;
         when Unsubscribe  => null;
      end case;
   end record;

   use Typed.Conversions;

   function On_Next (V : Typed.T) return Event is (On_Next, +V);

   function On_Completed return Event is (Kind => On_Completed);

   function On_Error (E : Errors.Occurrence) return Event is (On_Error, E);

   function On_Error (E : Ada.Exceptions.Exception_Occurrence) return Event is (On_Error, Errors.Create (E));

   function Unsubscribe return Event is (Kind => Unsubscribe);

   function Value (E : Event) return Typed.T is (+E.V);

   function Error (E : Event) return Errors.Occurrence is (E.E);

end Rx.Impl.Events;
