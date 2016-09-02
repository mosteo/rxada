with Rx.Sources.Stateless;

package body Rx.Empty is

   type Void is null record;

   procedure On_Subscribe (State    : Void;
                           Observer : in out Typed.Observer) is null;

   package Empty_Sources is new Rx.Sources.Stateless (Typed, Void);

   -----------
   -- Empty --
   -----------

   function Empty return Typed.Observable is
   begin
      return Empty_Sources.Create (Void'(null record));
   end Empty;

   -----------
   -- Never --
   -----------

   package Never_Sources is new Rx.Sources.Stateless (Typed, Void, On_Subscribe, Completes => False);

   function Never return Typed.Observable is
   begin
      return Never_Sources.Create (Void'(null record));
   end Never;

   -----------
   -- Error --
   -----------

   procedure On_Subscribe_Error (Error    : Errors.Occurrence;
                                 Observer : in out Typed.Observer)
   is
      E : Errors.Occurrence := Error;
   begin
      Observer.On_Error (E);
   end On_Subscribe_Error;

   package Error_Sources is new Rx.Sources.Stateless (Typed, Errors.Occurrence, On_Subscribe_Error, Completes => False);

   function Error
     (E : Rx.Errors.Occurrence)
      return Typed.Observable
   is
   begin
      return Error_Sources.Create (E);
   end Error;

   -----------
   -- Error --
   -----------

   function Error
     (E : Ada.Exceptions.Exception_Occurrence)
      return Typed.Observable
   is
      Err : Errors.Occurrence;
   begin
      Err.Fill (E);
      return Error (Err);
   end Error;

end Rx.Empty;
