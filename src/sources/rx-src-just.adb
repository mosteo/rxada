with Rx.Src;
--  with rx.src.stateless; -- Next step

package body Rx.Just is

   type Observable is new Typed.Producers.Observable with record
      Value : Typed.Type_Traits.D;
   end record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Typed.Consumers.Observer'Class) is
   begin
      Consumer.On_Next (Typed.Type_Traits.To_Indefinite (Producer.Value));
      Consumer.On_Completed;
   end Subscribe;

   function Create (V : Typed.Type_Traits.T) return Typed.Producers.Observable'Class is
   begin
      return Observable'(Typed.Producers.Observable with Value => Typed.Type_Traits.To_Definite (V));
   end Create;

end Rx.Just;
