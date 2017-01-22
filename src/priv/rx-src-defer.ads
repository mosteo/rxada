with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Src.Defer is

   package Factories renames Typed.Factories;

   function Create (F : Factories.Observable_Factory'Class) return Typed.Observable;
   --  The factory code won't be invoked until actual subscription time

   function Create (F : Factories.Observable_Factory_Func) return Typed.Observable;

end Rx.Src.Defer;
