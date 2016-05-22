package body Rx.Just is

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
end Rx.Just;
