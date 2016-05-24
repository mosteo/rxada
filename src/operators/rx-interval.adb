package body Rx.Interval is

   Instance : aliased Observable;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (O : in out Observable;
      S : access Output.Observer'Class)
   is
   begin
      -- In thread, this is wrong for the time being!
      delay First_Pause;
      S.OnNext (1);
      loop
         delay Pause;
         O.Next := O.Next + 1;
         S.OnNext (O.Next);
      end loop;
   end Subscribe;

begin
   Output.Instance := Instance'Access;
end Rx.Interval;
