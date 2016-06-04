with Rx.Just;

package body Rx.Observable is

   package RxJust is new Rx.Just (Producers);

   function Just (V : T) return Producers.Observable'Class is
   begin
      return RxJust.Create (V);
   end Just;

end Rx.Observable;
