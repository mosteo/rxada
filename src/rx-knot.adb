with Rx.Just;
with Rx.Subscribe;

package body Rx.Knot is

   package RxJust is new Rx.Just (Binding);
   
   package RxSubscribe is new Rx.Subscribe (Binding);

   function Just (Val : T) return Observable'Class is
   begin
      return Knot.Observable'(Real => RxJust.Create (Val));
   end Just;
   
   overriding
   procedure Subscribe (O : in out Observable; 
                        S : access Binding.Observer'Class) is 
   begin
      O.Real.Subscribe (S);
   end Subscribe;
   
   procedure Subscribe (O : in out Observable;
                        A : Binding.Action := null)
   is
   begin
      O.Subscribe (RxSubscribe.Create (A));
   end;
                        
   
end Rx.Knot;
