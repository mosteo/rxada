with Ada.Exceptions;

with Rx.Errors;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Impl.Events is

   type Kinds is (On_Next, On_Complete , On_Error);

   type Event (Kind : Kinds) is private;

   function On_Next (V : Typed.T) return Event;

   function On_Complete  return Event;

   function On_Error (E : Errors.Occurrence) return Event;

   function On_Error (E : Ada.Exceptions.Exception_Occurrence) return Event;

   function Value (E : Event) return Typed.T
     with Pre => E.Kind = On_Next;

   function Error (E : Event) return Errors.Occurrence
     with Pre => E.Kind = On_Error;

private

   type Event (Kind : Kinds) is record
      case Kind is
         when On_Next      => V : Typed.D;
         when On_Error     => E : Errors.Occurrence;
         when On_Complete  => null;
      end case;
   end record;

   use Typed.Conversions;

   function On_Next (V : Typed.T) return Event is (On_Next, +V);

   function On_Complete  return Event is (Kind => On_Complete );

   function On_Error (E : Errors.Occurrence) return Event is (On_Error, E);

   function On_Error (E : Ada.Exceptions.Exception_Occurrence) return Event is (On_Error, Errors.Create (E));

   function Value (E : Event) return Typed.T is (+E.V);

   function Error (E : Event) return Errors.Occurrence is (E.E);

end Rx.Impl.Events;
