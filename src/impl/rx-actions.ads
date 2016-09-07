with Rx.Errors;

package Rx.Actions is

   pragma Preelaborate;

   type Proc0      is access procedure;
   type Proc_Error is access procedure (E : Errors.Occurrence);

   generic
      type T (<>) is private;
   package Typed is

      type Func1    is access function (V : T) return T;
      type Func1Str is access function (V : T) return String;

      type Proc1 is access procedure (V : T);

   end Typed;

end Rx.Actions;
