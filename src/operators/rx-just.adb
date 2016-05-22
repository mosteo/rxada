package body Rx.Just is

   Instance : aliased Observable := (V => Holder.Hold (Just.V));

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (O : in out Observable;
      S : access Output.Observer'Class)
   is
   begin
      S.OnNext (O.V.Element);
      S.OnCompleted;
   end Subscribe;

begin
   Output.Instance := Instance'Access;
   -- No memory allocation, so no leak... but I bet this is thoroughly broken for concurrent access
end Rx.Just;