with Rx.Preservers;

generic
   with package Operate is new Rx.Preservers (<>);
package Rx.Op.Filter is

   function Create (Filter : not null Operate.Typed.Actions.Filter1) return Operate.Preserver'Class;

end Rx.Op.Filter;
