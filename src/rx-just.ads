with Rx.Producers;

generic
   with package Producers is new Rx.Producers (<>);
package Rx.Just is

   type Observable is new Producers.Observable with private;

   function Create (V : Producers.T) return Producers.Observable'Class;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Producers.Consumers.Observer'Class);

private

   type Observable is new Producers.Observable with record
      Value : Producers.T;
   end record;

end Rx.Just;
