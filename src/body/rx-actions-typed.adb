package body Rx.Actions.Typed is

   type Countdown_Filter is new TFilter1 with record
      Remaining : Rx_Natural;
   end record;

   overriding function Check (Filter : in out Countdown_Filter; V : T) return Boolean;

   ---------------
   -- Countdown --
   ---------------

   function Countdown (Times : Rx_Natural) return TFilter1'Class is (Countdown_Filter'(Remaining => Times));

   -----------
   -- Check --
   -----------

   overriding function Check (Filter : in out Countdown_Filter; V : T) return Boolean is
      pragma Unreferenced (V);
   begin
      return Passed : constant Boolean := Filter.Remaining > 0 do
         if Passed then
            Filter.Remaining := Filter.Remaining - 1;
         end if;
      end return;
   end Check;

   -----------
   -- "not" --
   -----------

   type Negator is new TFilter1 with record
      Filter : HTFilter1;
   end record;

   -----------
   -- Check --
   -----------

   overriding function Check (This : in out Negator; V : T) return Boolean is
   begin
      return not This.Filter.Ref.Check (V);
   end Check;

   function "not" (Filter : TFilter1'Class) return TFilter1'Class is
     (Negator'(Filter => + Filter));

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

   -------------
   -- WTProc1 --
   -------------

   type WTProc1 (Proc : Proc1) is new TProc1 with null record;
   overriding procedure Call (Proc : in out WTProc1; V : T) is
   begin
      Proc.Proc (V);
   end Call;
   function Wrap (Proc : Proc1) return TProc1'Class is
      (WTProc1'(Proc => Proc));

end Rx.Actions.Typed;
