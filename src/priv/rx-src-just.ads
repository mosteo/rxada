with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Src.Just is

   function Create (V : Typed.T) return Typed.Observable'Class;

end Rx.Src.Just;
