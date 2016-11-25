with Rx.Preserve;

generic
   with package Operate is new Rx.Preserve (<>);
package Rx.Op.Filter is

   function Create (Filter : not null Operate.Typed.Actions.Filter1) return Operate.Operator'Class;

end Rx.Op.Filter;
