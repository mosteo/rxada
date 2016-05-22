package body Rx.From is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (O : in out Observable;
      S : access Output.Observer'Class)
   is
   begin
      for I in O.VA'Range loop
         S.OnNext (O.VA (I));
      end loop;
      S.OnCompleted;
   end Subscribe;

begin
   Output.Instance := Instance'Access;
end Rx.From;
