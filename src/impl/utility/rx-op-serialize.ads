with Rx.Operate;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.Op.Serialize is

   function Create return Operate.Preserver;

end Rx.Op.Serialize;
