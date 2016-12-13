package body Rx.Dispatchers.Immediate is

   --------------
   -- Schedule --
   --------------

   overriding procedure Schedule
     (Where : in out Dispatcher;
      What  : Runnable'Class;
      Time  : Ada.Calendar.Time := Ada.Calendar.Clock)
   is
      pragma Unreferenced (Where);
   begin
      delay until Time;
      What.Run;
   end Schedule;

end Rx.Dispatchers.Immediate;
