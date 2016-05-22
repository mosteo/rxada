with Rx.Operator;

generic
   with package   Observable is new Rx.Operator (<>);
   with procedure OnNext (V : Observable.T) is null;
   with procedure OnCompleted is null;
package Rx.Subscribe is

   pragma Elaborate_Body;

end Rx.Subscribe;
