package body Rx.Producers.Composite is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Observable;
      Consumer : Observer'Class)
   is
   begin
      Producer.Observers.Append (Consumer);
   end Subscribe;

end Rx.Producers.Composite;
