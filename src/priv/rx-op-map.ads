with Rx.Transformers;

generic
   with package Typed is new Rx.Transformers (<>);
package Rx.Op.Map with Preelaborate is

   function Create (F : Typed.Actions.Func1) return Typed.Operator'Class;

end Rx.Op.Map;
