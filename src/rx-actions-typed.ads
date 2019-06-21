generic
   type T (<>) is private;
package Rx.Actions.Typed with Preelaborate is

   type Func0 is access function return T;
   type TFunc0 is interface;
   function Get (Func : in out TFunc0) return T is abstract;
   function Wrap (Func : Func0) return TFunc0'Class;

   type Func1 is access function (V : T) return T;

   type Func1Str is access function (V : T) return String;
   type TFunc1Str is interface;
   function Convert (Func : in out TFunc1Str; V : T) return String is abstract;
   function Wrap (Func : Func1Str) return TFunc1Str'Class;

   type Proc1 is access procedure (V : T);
   type TProc1 is interface;
   procedure Call (Proc : in out TProc1; V : T) is abstract;
   function Wrap (Proc : Proc1) return TProc1'Class;

   type Filter1  is access function (V : T) return Boolean;
   type TFilter1 is interface;
   function Check (Filter : in out TFilter1; V : T) return Boolean is abstract;
   function Wrap (Filter : Filter1) return TFilter1'Class;

   type Comparator is access function (L, R : T) return Boolean;

   --  Holders

   package Func0_Holders is new Rx.Tools.Holders (TFunc0'Class, "func0");
   type HTFunc0 is new Func0_Holders.Definite with null record;

   package Func1Str_Holders is new Rx.Tools.Holders (TFunc1Str'Class, "func1str");
   type HTFunc1Str is new Func1Str_Holders.Definite with null record;

   package Filter1_Holders is new Rx.Tools.Holders (TFilter1'Class, "filter1");
   type HTFilter1 is new Filter1_Holders.Definite with null record;

   package Proc1_Holders is new Rx.Tools.Holders (TProc1'Class, "proc1");
   type HTProc1 is new Proc1_Holders.Definite with null record;

   --  Predefined actions

   function Always_Pass return TFilter1'Class;
   --  Trivial filter that always returns true

   function Countdown (Times : Rx_Natural) return TFilter1'Class;
   --  Filter that passes Times times and then fails forever

   function "not" (Filter : TFilter1'Class) return TFilter1'Class;
   function "not" (Filter : Filter1) return TFilter1'Class;
   --  Negates the result of some filter

   function Negate (Filter : TFilter1'Class) return TFilter1'Class renames "not";

private

   function Always_True (Unused : T) return Boolean is (True);

   function Always_Pass return TFilter1'Class is (Wrap (Always_True'Access));

   function "not" (Filter : Filter1) return TFilter1'Class is
      (Negate (Wrap (Filter)));

end Rx.Actions.Typed;
