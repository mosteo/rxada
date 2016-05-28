package body Rx.Subscribe is

   use type Binding.Action;
   
   function Create (A : Binding.Action) return not null access Observer is
   begin
      return new Observer'(A => A);
   end Create;

   overriding
   procedure OnNext (This : in out Observer; V : Binding.T) is
   begin
      if This.A /= null then
         This.A (V);
      end if;
   end OnNext;

end Rx.Subscribe;
