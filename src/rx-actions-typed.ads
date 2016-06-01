with Rx.Values;
with Rx.Values.Typed;

generic
   with package Values is new Rx.Values.Typed (<>);
package Rx.Actions.Typed is

   subtype Value is Rx.Values.Value'Class;

   type Func1 is abstract new Actions.Func1 with null record;
   function Call (F : Func1; V : Values.T) return Value'Class is abstract;

   type Raw_Proc1 is access procedure (V : Values.T);

   type Proc1 (Raw : Raw_Proc1) is new Actions.Proc1 with null record;
   overriding
   procedure Call (P : Proc1; V : Value'Class);

end Rx.Actions.Typed;
