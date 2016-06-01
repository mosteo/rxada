with Rx.Base;
with Rx.Subscribers;

package Rx.Subscribe is

   function As (Proc1 : Base.A.Proc1'Class) return Subscribers.Observer'Class;

end Rx.Subscribe;
