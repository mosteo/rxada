with Rx.Preserve;

generic
   with package Operate is new Rx.Preserve (<>);
package Rx.Op.Serialize is

   function Create return Operate.Preserver;

end Rx.Op.Serialize;
