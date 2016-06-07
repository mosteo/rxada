with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Just is

   type Observable is new Typed.Producers.Observable with private;

   function Create (V : Typed.Type_Traits.T) return Typed.Producers.Observable'Class;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Typed.Consumers.Observer'Class);

private

   type Observable is new Typed.Producers.Observable with record
      Value : Typed.Type_Traits.D;
   end record;

end Rx.Just;
