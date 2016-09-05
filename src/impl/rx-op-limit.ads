with Rx.Operate;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.Op.Limit is

   function Create (Limit : Natural) return Operate.Operator'Class;

end Rx.Op.Limit;
