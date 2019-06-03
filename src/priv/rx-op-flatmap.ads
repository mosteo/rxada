with Rx.Impl.Transformers;
with Rx.Schedulers;

generic
   with package Transformer is new Rx.Impl.Transformers (<>);
package Rx.Op.Flatmap is

   function Create (Func      : Transformer.Actions.Inflater1;
                    Scheduler : Schedulers.Scheduler := Schedulers.Immediate)
                    return Transformer.Operator'Class;

   function Create (Func      : Transformer.Actions.TInflater1'Class;
                    Scheduler : Schedulers.Scheduler := Schedulers.Immediate)
                    return Transformer.Operator'Class;

private

   function Create (Func      : Transformer.Actions.Inflater1;
                    Scheduler : Schedulers.Scheduler := Schedulers.Immediate)
                    return Transformer.Operator'Class is
      (Create (Transformer.Actions.Wrap (Func), Scheduler));

end Rx.Op.Flatmap;
