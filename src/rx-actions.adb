package body Rx.Actions is

   type WTFilter0 (Filter : Filter0) is new TFilter0 with null record;

   overriding function Check (Filter : in out WTFilter0) return Boolean is
      (Filter.Filter.all);

   ----------
   -- Wrap --
   ----------

   function Wrap (Check : Filter0) return TFilter0'Class is
   begin
      return WTFilter0'(TFilter0 with Check);
   end Wrap;

end Rx.Actions;
