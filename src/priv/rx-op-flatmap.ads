with Rx.Impl.Transformers;

generic
   with package Transformer is new Rx.Impl.Transformers (<>);
package Rx.Op.Flatmap is

   function Create (Func   : Transformer.Actions.Flattener1)
                    return Transformer.Operator'Class;

end Rx.Op.Flatmap;
