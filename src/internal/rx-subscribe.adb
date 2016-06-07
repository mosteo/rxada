package body Rx.Subscribe is

   type Proc1_Observer is new Typed.Consumers.Observer with record
      On_Next : Typed.Actions.Proc1;
   end record;

   overriding
   procedure On_Next (This : in out Proc1_Observer; V : Typed.Type_Traits.T) is
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
      return Proc1_Observer'(On_Next => Proc1);
   end As;

end Rx.Subscribe;
