with Rx.Impl.Transformers;

generic
   with package Typed is new Rx.Impl.Transformers (<>);
package Rx.Op.Map with Preelaborate is

   function Create (F : Typed.Actions.Func1) return Typed.Operator'Class;

end Rx.Op.Map;
