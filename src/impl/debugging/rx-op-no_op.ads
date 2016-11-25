with Rx.Preserve;

generic
   with package Operate is new Rx.Preserve (<>);
package Rx.Op.No_Op is

   function Create return Operate.Operator'Class;

end Rx.Op.No_Op;
