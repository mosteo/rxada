with Ada.Exceptions;

with Rx.Errors;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Src.Empty is

   function Empty return Typed.Observable;

   function Never return Typed.Observable;

   function Error (E : Rx.Errors.Occurrence)                return Typed.Observable;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Typed.Observable;

end Rx.Src.Empty;
