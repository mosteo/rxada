with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Serialize is

   function Create return Operate.Operator'Class;

end Rx.Op.Serialize;
