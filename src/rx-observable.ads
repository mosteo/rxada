with Rx.Actions;
with Rx.Actions.Typed;
with Rx.Base;
with Rx.subscribers;
with Rx.Values.Typed;

generic
   type T (<>) is private;
package Rx.Observable is

   package Values   is new Rx.Values.Typed (T);
   package TActions is new Rx.Actions.Typed (Values); use type TActions.Raw_Proc1;

   type Observable is new Base.Observable with null record;

   function Just (V : T) return Base.Observable'Class;

   function Proc (P : TActions.Raw_Proc1) return Rx.Actions.Proc1'Class is (TActions.Proc1'(Raw => P));

private

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Subscribers.Observer'Class) is null;

end Rx.Observable;
