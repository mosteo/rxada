with Rx.Impl.Transformers;

generic
   with package Transformer is new Rx.Impl.Transformers (<>);
package Rx.Op.Flatmap is

   function Create (Func      : Transformer.Actions.Inflater1)
                    return Transformer.Operator'Class;

   function Create (Func      : Transformer.Actions.TInflater1'Class)
                    return Transformer.Operator'Class;

private

   function Create (Func      : Transformer.Actions.Inflater1)
                    return Transformer.Operator'Class is
      (Create (Transformer.Actions.Wrap (Func)));

end Rx.Op.Flatmap;
