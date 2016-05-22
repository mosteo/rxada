with Rx.Producer;

generic
   with package   Bind is new Rx.Producer (<>);
   with procedure OnSubscribe (S : access Bind.Observer'Class) is null;
   with procedure OnNext is null;
package Rx.Operator is

   Observable : access Bind.Observable'Class;

end Rx.Operator;
