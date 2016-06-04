with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Subscribe is

   function As (Proc1 : Typed.Actions.Proc1) return Typed.Consumers.Observer'Class;

end Rx.Subscribe;
