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

   type Filter1  is access function (V : T) return Boolean;
   type TFilter1 is interface;
   function Check (Filter : in out TFilter1; V : T) return Boolean is abstract;
   function Wrap (Filter : Filter1) return TFilter1'Class;

   --  Holders

   package Func0_Holders is new Rx.Holders (TFunc0'Class);
   type HTFunc0 is new Func0_Holders.Definite with null record;

   package Func1Str_Holders is new Rx.Holders (TFunc1Str'Class);
   type HTFunc1Str is new Func1Str_Holders.Definite with null record;

   package Filter1_Holders is new Rx.Holders (TFilter1'Class);
   type HTFilter1 is new Filter1_Holders.Definite with null record;

   --  Predefined actions

   Always_Pass : constant TFilter1'Class;
   --  Trivial filter that always returns true

private

   function Always_True (V : T) return Boolean is (True);

   Always_Pass : constant TFilter1'Class := Wrap (Always_True'Access);

end Rx.Actions.Typed;
