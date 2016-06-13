with Rx.Debug;
with Rx.Errors;

package body Rx.Subscribe is

   type Obs is new Typed.Consumers.Observer and Typed.Consumers.Sink with record
      On_Next : Typed.Actions.Proc1;
   end record;

   pragma Compile_Time_Warning (True, "Methods unimplemented");
   overriding procedure On_Next (This : in out Obs; V : Typed.Type_Traits.T);
   overriding procedure On_Completed (This : in out Obs) is null;
   overriding procedure On_Error (This : in out Obs; Error : in out Errors.Occurrence);

   overriding procedure On_Error (This : in out Obs; Error : in out Errors.Occurrence) is
   begin
      Debug.Print (Error.Get_Exception.all);
      Error.Reraise;
   end On_Error;

   overriding
   procedure On_Next (This : in out Obs; V : Typed.Type_Traits.T) is
      use Typed.Actions;
   begin
      if This.On_Next /= null then
         This.On_Next (V);
      end if;
   end On_Next;

   --------
   -- As --
   --------

   function As (Proc1 : Typed.Actions.Proc1) return Typed.Consumers.Observer'Class is
   begin
      return Obs'(On_Next => Proc1);
   end As;

end Rx.Subscribe;
