with Rx.Operate;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.No_Op is

   function Create return Operate.Operator;

end Rx.No_Op;
