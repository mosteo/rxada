generic
   type T (<>) is private;
package Rx.Actions.Typed with Preelaborate is

   type Func1 is access function (V : T) return T;

   type Func1Str is access function (V : T) return String;
   type TFunc1Str is interface;
   function Convert (Func : in out TFunc1Str; V : T) return String is abstract;
   function Wrap (Func : Func1Str) return TFunc1Str'Class;

   type Proc1 is access procedure (V : T);

   type Filter1 is access function (V : T) return Boolean;

   --  Holders

   package Func1Str_Holders is new Rx.Holders (TFunc1Str'Class);
   type HTFunc1Str is new Func1Str_Holders.Definite with null record;

   --  Predefined actions

end Rx.Actions.Typed;
