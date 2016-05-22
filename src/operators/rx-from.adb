package body Rx.From is

   Instance : aliased Observable := (Last => VA'Length, VA => From.VA);

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
