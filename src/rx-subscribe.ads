generic
   with package Observable is new Rx.Base (<>);
   with procedure OnNext (V : Observable.T) is null;
   with procedure OnCompleted is null;
package Rx.Subscribe is

   pragma Elaborate_Body;

end Rx.Subscribe;
