package body Rx.Just is

   type Observable is new Typed.Producers.Observable with record
      Value : Typed.Holders.Definite;
   end record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Typed.Consumers.Observer'Class) is
   begin
      Consumer.On_Next (Producer.Value.CRef);
      Consumer.On_Completed;
   end Subscribe;

   function Create (V : Typed.T) return Typed.Producers.Observable'Class is
      use Typed.Holders;
   begin
      return Observable'(Typed.Producers.Observable with Value => +V);
   end Create;

end Rx.Just;
