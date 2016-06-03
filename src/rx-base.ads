with Rx.Actions;
with Rx.Consumers;
with Rx.Producers;

package Rx.Base is

   --  Proper observable
   type Observable is abstract new Producers.Observable with private;

   --   function Map (O : Observable; F : A.Func1'Class) return Observable'Class;

   function "&" (L : Observable'Class; R : Observable'Class) return Observable'Class is (R);

   procedure Subscribe (O        : Observable;
                        On_Next  : Rx.Actions.Proc1'Class := Rx.Actions.No_Op);

private

   type Observable is abstract new Producers.Observable with record
      Child : Consumers.Holder; -- Downstream consumer, used during actual data propagation
   end record;

end Rx.Base;
