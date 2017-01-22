with Rx.Impl.Preservers;
with Rx.Impl.Typed;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
   with package Samplers is new Rx.Impl.Typed (<>);
package Rx.Op.Sample is

   type Policies is (Keep_First, Keep_Last);

   function Create (Policy  : Policies;
                    Sampler : Samplers.Observable'Class) return Operate.Operator'Class;

end Rx.Op.Sample;
