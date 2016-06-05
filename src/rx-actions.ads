generic
   type T (<>) is private;
package Rx.Actions is

   pragma Preelaborate;

   type Func1 is access function (V : T) return T;

   type Proc1 is access procedure (V : T);

end Rx.Actions;
