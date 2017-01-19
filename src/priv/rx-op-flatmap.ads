with Rx.Transformers;

generic
   with package Transformer is new Rx.Transformers (<>);
package Rx.Op.Flatmap is

   type Policies is -- For the mixing of the generated observables
     (Merge, 	-- Just relay as they come
      Sequence, -- Force sequencing of observables
      Switch);  -- Drop previous observable once a new one arrives

   function Create (Func   : Transformer.Actions.Flattener1;
                    Policy : Policies := Merge) return Transformer.Operator'Class;

end Rx.Op.Flatmap;
