with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Filter is

   function Create (Filter : not null Operate.Typed.Actions.Filter1) return Operate.Operator'Class;

end Rx.Op.Filter;
