with Rx.Impl.Preservers;
with Rx.Typed;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
   with package Samplers is new Rx.Typed (<>);
package Rx.Op.Sample is

   type Policies is (Keep_First, Keep_Last);

   function Create (Policy  : Policies;
                    Sampler : Samplers.Observable'Class) return Operate.Operator'Class;

end Rx.Op.Sample;
