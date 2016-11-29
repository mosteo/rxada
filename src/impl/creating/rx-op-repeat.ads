with Rx.Actions;
with Rx.Preservers;

generic
   with package Operate is new Rx.Preservers (<>);
package Rx.Op.Repeat is

   function Repeat_Forever return Operate.Preserver'Class;

   function Repeat (Times : Positive) return Operate.Preserver'Class;

   function While_Do (Check : Actions.TFilter0'Class) return Operate.Preserver'Class;
   --  The check is performed before each repetition

   function Repeat_Until (Check : Actions.TFilter0'Class) return Operate.Preserver'Class;
   --  The check is performed after each repetition

end Rx.Op.Repeat;
