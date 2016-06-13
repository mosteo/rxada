with Rx.Operate;

generic
   with package Operate is new Rx.Operate (<>);
package Rx.Print is

   type Func1 is access not null function (V : Operate.T) return String;

   function Create (Func : Func1 := null) return Operate.Operator;
   --  If null, "print" will be printed

end Rx.Print;
