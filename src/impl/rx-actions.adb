package body Rx.Actions is

   ---------------
   --  WTProc0  --
   ---------------

   type WTProc0 (Proc : Proc0) is new TProc0 with null record;
   overriding procedure Run (Proc : in out WTProc0) is
   begin
      Proc.Proc.all;
   end Run;
   function Wrap (Proc : Proc0) return TProc0'Class is (WTProc0'(Proc => Proc));

   ---------------
   -- WTFilter0 --
   ---------------

   type WTFilter0 (Filter : Filter0) is new TFilter0 with null record;
   overriding function Check (Filter : in out WTFilter0) return Boolean is
   begin
      return Filter.Filter.all;
   end Check; -- Cannot be expression function because of GNAT bug
   function Wrap (Check : Filter0) return TFilter0'Class is (WTFilter0'(Filter => Check));

   -------------
   -- Counter --
   -------------

   type Counter (Times : Positive) is new TFilter0 with record
      Current : Natural := 0;
   end record;

   overriding function Check (This : in out Counter) return Boolean is
   begin
      This.Current := This.Current + 1;
      return This.Current >= This.Times;
   end Check;

   function Count (Times : Positive) return TFilter0'Class is
     (Counter'(Times  => Times,
               others => <>));

   -----------
   -- "not" --
   -----------

   type Negator is new TFilter0 with record
      Filter : HTFilter0;
   end record;

   -----------
   -- Check --
   -----------

   overriding function Check (This : in out Negator) return Boolean is
   begin
      return not This.Filter.Ref.Check;
   end Check;

   function "not" (Filter : TFilter0'Class) return TFilter0'Class is
     (Negator'(Filter => + Filter));

end Rx.Actions;
