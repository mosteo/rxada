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
      if Time > Ada.Calendar.Clock then
         raise Constraint_Error with "Future scheduling in immediate scheduler not allowed";
      end if;
      What.Run;
   end Schedule;

end Rx.Dispatchers.Immediate;
