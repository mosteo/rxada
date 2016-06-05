with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Just is

   function Create (V : Typed.T) return Typed.Producers.Observable'Class;

end Rx.Just;
