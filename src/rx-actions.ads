with Rx.Errors;
with Rx.Holders;

package Rx.Actions is

   pragma Preelaborate;

   type Proc0      is access procedure;
   type Proc_Error is access procedure (E : Errors.Occurrence);

   type Filter0 is access function return Boolean;

   type TFilter0 is abstract tagged null record;
   function Check (Filter : in out TFilter0) return Boolean is abstract;
   --  Variant with state

   function Wrap (Check : Filter0) return TFilter0'Class;

   generic
      type T (<>) is private;
   package Typed is

      type Func1    is access function (V : T) return T;
      type Func1Str is access function (V : T) return String;

      type Proc1 is access procedure (V : T);

      type Filter1 is access function (V : T) return Boolean;

   end Typed;

   --  Holders for the tagged variants follow

   package Filter0_Holders is new Rx.Holders (TFilter0'Class);
   type HTFilter0 is new Filter0_Holders.Definite with null record;

end Rx.Actions;
