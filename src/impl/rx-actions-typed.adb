package body Rx.Actions.Typed is

   -------------------
   --  WTFunc1Str  ---
   -------------------

   type WTFunc1Str (Func : Func1Str) is new TFunc1Str with null record;
   overriding function Convert (Func : in out WTFunc1Str; V : T) return String is (Func.Func (V));
   function Wrap (Func : Func1Str) return TFunc1Str'Class is
   begin
      return WTFunc1Str'(Func => Func);
   end Wrap; -- Can't be expression function because of gnat bug

   type WTFilter1 (Filter : Filter1) is new TFilter1 with null record;
   overriding function Check (Filter : in out WTFilter1; V : T) return Boolean is (Filter.Filter (V));
   function Wrap (Filter : Filter1) return TFilter1'Class is
   begin
      return WTFilter1'(Filter => Filter);
   end Wrap;


end Rx.Actions.Typed;
