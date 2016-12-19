with Rx.Transform;

generic
   with package Typed is new Rx.Transform (<>);
package Rx.Op.Flatmap with Preelaborate is

   type Policies is -- For the mixing of the generated observables
     (Merge, 	-- Just relay as they come
      Sequence, -- Force sequencing of observables
      Switch);  -- Drop previous observable once a new one arrives

   function Flatten (Func   : Typed.Actions.Flattener1;
                     Policy : Policies) return Typed.Transformer;

end Rx.Op.Flatmap;
