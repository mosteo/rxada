with Rx.Preservers;

generic
   with package Operate is new Rx.Preservers (<>);
package Rx.Op.No_Op is

   function Create return Operate.Operator'Class;

end Rx.Op.No_Op;
