generic
   type T (<>) is private;
package Rx.Actions is

   pragma Preelaborate;

   type Proc1 is access procedure (V : T);

end Rx.Actions;
