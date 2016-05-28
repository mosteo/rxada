with Rx.Debug;
package body Rx.Just is

   overriding 
   procedure Subscribe
     (O : in out Observable;
      S : access Binding.Observer'Class)
   is
   begin
      S.OnNext (O.V.Element);
      S.OnCompleted;
   end Subscribe;
   
   function Create (Val : T) return not null access Binding.Observable'Class is
   begin
      return new Observable'(V => Holder.To_Holder (Val));
   end Create;

   
end Rx.Just;
