with Rx.Actions;
with Rx.Producers;

package Rx.Base is

   --  Confort zone
   package A renames Rx.Actions;

   --  Proper observable
   type Observable is abstract new Producers.Observable with null record;

   function Map (O : Observable; F : A.Func1'Class) return Observable'Class;

   procedure Subscribe (O : Observable; P : A.Proc1'Class := Actions.No_Op);

end Rx.Base;
