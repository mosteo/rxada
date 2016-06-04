package body Rx.Map is

   type Op (F : Typed.Func1) is new Typed.Operator with record
      Observer : Typed.Into.Consumers.Holder;
   end record;

   overriding
   procedure Subscribe (Producer : in out Op;
                        Consumer : Typed.Into.Consumers.Observer'Class)
   is
      Parent : Typed.From.Producers.Observable'Class := Producer.Get_Parent;
   begin
      Producer.Observer := Typed.Into.Consumers.To_Holder (Consumer);
      Parent.Subscribe (Producer);
   end Subscribe;

   overriding
   procedure OnNext (This : in out Op; V : Typed.From.T) is
   begin
      This.Observer.CRef.OnNext (F (V));
   end OnNext;

   ------------
   -- Create --
   ------------

   function Create (F : Typed.Func1) return Typed.Operator'Class is
   begin
      return Op'(Typed.Operator with F => F, others => <>);
   end Create;

end Rx.Map;
