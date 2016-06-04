with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Just is

   type Observable is new Typed.Producers.Observable with private;

   function Create (V : Typed.T) return Typed.Producers.Observable'Class;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Typed.Consumers.Observer'Class);

private

   type Observable is new Typed.Producers.Observable with record
      Value : Typed.Holders.Definite;
   end record;

end Rx.Just;
