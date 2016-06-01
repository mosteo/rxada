with Rx.Values;

package body Rx.Subscribe is

   type Proc1_Observer is new Subscribers.Observer with record
      On_Next : Base.A.H_Proc1.Holder;
   end record;

   overriding
   procedure OnNext (This : Proc1_Observer; V : Rx.Values.Value'Class) is
   begin
      This.On_Next.Constant_Reference.Call (V);
   end OnNext;

   --------
   -- As --
   --------

   function As (Proc1 : Base.A.Proc1'Class) return Subscribers.Observer'Class is
   begin
      return Proc1_Observer'(On_Next => Base.A.H_Proc1.To_Holder (Proc1));
   end As;

end Rx.Subscribe;
