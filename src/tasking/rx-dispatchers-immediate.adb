package body Rx.Dispatchers.Immediate is

   --------------
   -- Schedule --
   --------------

   overriding procedure Schedule
     (Where : in out Dispatcher;
      What  : in out Runnable'Class;
      After : Duration := 0.0)
   is
   begin
      delay After;
      What.Run;
   end Schedule;

end Rx.Dispatchers.Immediate;
