package body Rx.Map is

   type Op (F : Typed.Func1) is new Typed.Operator with null record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Consumers.Observer'Class) is abstract;

   ------------
   -- Create --
   ------------

   function Create (F : Typed.Func1) return Typed.Operator'Class is
   begin
      return Op'(Typed.Operator with F => F);
   end Create;

end Rx.Map;
