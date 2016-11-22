with Rx.Transform;

generic
   with package Typed is new Rx.Transform (<>);
package Rx.Op.Map with Preelaborate is

   function Create (F : Typed.Actions.Func1) return Typed.Transformer;

end Rx.Op.Map;
