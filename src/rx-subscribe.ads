with Rx.Actions;
with Rx.Consumers;

package Rx.Subscribe is

   function As (Proc1 : Actions.Proc1'Class) return Consumers.Observer'Class;

end Rx.Subscribe;
