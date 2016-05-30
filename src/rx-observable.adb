with Rx.Just;

package body Rx.Observable is

   ----------
   -- Just --
   ----------

   function Just (V : T) return Observable'Class is
   begin
      return Just (Values.Wrap (V));
   end Just;

   overriding
   function Just (V : I.Value'Class) return Observable is
   begin
      return Rx.Just.Create (V);
   end Just;

end Rx.Observable;
