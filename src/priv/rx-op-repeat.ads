with Rx.Actions;
with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Repeat is

   function Repeat_Forever return Operate.Operator'Class;

   function Repeat (Times : Rx_Integer) return Operate.Operator'Class;

   function While_Do (Check : Actions.TFilter0'Class) return Operate.Operator'Class;
   --  The check is performed before each repetition

   function Repeat_Until (Check : Actions.TFilter0'Class) return Operate.Operator'Class;
   --  The check is performed after each repetition

end Rx.Op.Repeat;
