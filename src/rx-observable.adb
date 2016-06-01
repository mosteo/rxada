with Rx.Just;

package body Rx.Observable is

   ----------
   -- Just --
   ----------

   function Just (V : T) return Base.Observable'Class is
   begin
      return Rx.Just.Create (Values.Wrap (V));
   end Just;

end Rx.Observable;
