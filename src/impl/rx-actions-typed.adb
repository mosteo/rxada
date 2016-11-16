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


end Rx.Actions.Typed;
