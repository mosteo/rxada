with Rx.Errors;
with Rx.Holders;

package Rx.Actions is

   pragma Preelaborate;

   type Proc0      is access procedure;
   type Proc_Error is access procedure (E : Errors.Occurrence);
   type TProc0 is interface;
   procedure Run (Proc : in out TProc0) is abstract;
   function Wrap (Proc : Proc0) return TProc0'Class;

   type Filter0 is access function return Boolean;
   type TFilter0 is interface;
   function Check (Filter : in out TFilter0) return Boolean is abstract;
   function Wrap (Check : Filter0) return TFilter0'Class;

   generic
      type T (<>) is private;
   package Typed is

      type Func1    is access function (V : T) return T;

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

   end Typed;

   --  Holders for the tagged variants follow

   package Proc0_Holders is new Rx.Holders (TProc0'Class);
   type HTProc0 is new Proc0_Holders.Definite with null record;

   package Filter0_Holders is new Rx.Holders (TFilter0'Class);
   type HTFilter0 is new Filter0_Holders.Definite with null record;

   --  Predefined actions follow

   function Count (Times : Positive) return TFilter0'Class;
   --  At and after the Times-nth call it will return true
   --  E.g. for Times = 3, Check returns False, False, True

   function "not" (Filter : TFilter0'Class) return TFilter0'Class;
   --  Negates the result of some filter

   function Negate (Filter : TFilter0'Class) return TFilter0'Class renames "not";

end Rx.Actions;
