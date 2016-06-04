with Rx.Actions;
with Rx.Consumers;

generic
   type T is private;
package Rx.Subscribe is

   function As (Proc1 : Actions.Proc1'Class) return Consumers.Observer'Class;

end Rx.Subscribe;
