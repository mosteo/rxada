package body Rx.Just is

   function Create (V : Producers.T) return Producers.Observable'Class is
   begin
      return Observable'(Producers.Observable with Value => V);
   end Create;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Producers.Consumers.Observer'Class) is
   begin
      Consumer.OnNext (Producer.Value);
      Consumer.OnCompleted;
   end Subscribe;

end Rx.Just;
