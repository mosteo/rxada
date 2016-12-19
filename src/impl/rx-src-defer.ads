with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Src.Defer is

   type Factory is interface;

   function On_Subscribe (F : Factory) return Typed.Observable is abstract;

   type Factory_Func is access function return Typed.Observable;

   function Create (F : Factory'Class) return Typed.Observable;
   --  The factory code won't be invoked until actual subscription time

   function Create (F : Factory_Func) return Typed.Observable;

end Rx.Src.Defer;
