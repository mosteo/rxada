with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.No_Op is

   function Create return Preserver.Operator'Class;

end Rx.Op.No_Op;
