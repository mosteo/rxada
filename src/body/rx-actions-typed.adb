package body Rx.Actions.Typed is

   -- These can't be expression functions because of gnat bug

   ---------------
   --  WTFunc0  --
   ---------------

   type WTFunc0 (Func : Func0) is new TFunc0 with null record;
   overriding function Get (Func : in out WTFunc0) return T is (Func.Func.all);

   function Wrap (Func : Func0) return TFunc0'Class is
   begin
      return WTFunc0'(Func => Func);
   end Wrap;

   -------------------
   --  WTFunc1Str  ---
   -------------------

   type WTFunc1Str (Func : Func1Str) is new TFunc1Str with null record;
   overriding function Convert (Func : in out WTFunc1Str; V : T) return String is (Func.Func (V));
   function Wrap (Func : Func1Str) return TFunc1Str'Class is
   begin
      return WTFunc1Str'(Func => Func);
   end Wrap;

   -----------------
   --  WTFilter1  --
   -----------------

   type WTFilter1 (Filter : Filter1) is new TFilter1 with null record;
   overriding function Check (Filter : in out WTFilter1; V : T) return Boolean is (Filter.Filter (V));
   function Wrap (Filter : Filter1) return TFilter1'Class is
   begin
      return WTFilter1'(Filter => Filter);
   end Wrap;


end Rx.Actions.Typed;
