with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Take is

   package Actions renames Operate.Typed.Actions;

   function Create (Pass : Actions.TFilter1'Class; Emit_Last : Boolean) return Operate.Operator'Class;
   --  Emit_Last is used to differentiate While and Until, since the former won't emit the one failing Pass,
   --  whereas Until will

   function Take_Count (Count : Rx_Natural) return Operate.Operator'Class;

   function Take_While (Check : Actions.TFilter1'Class) return Operate.Operator'Class;

   function Take_Until (Check : Actions.TFilter1'Class) return Operate.Operator'Class;

   function Create (During : Duration) return Operate.Operator'Class;

end Rx.Op.Take;
