package Rx.Dispatchers.Immediate is

   use type Ada.Calendar.Time;

   type Dispatcher is limited new Dispatchers.Dispatcher with private;

   --  Schedule a code to be run at a certain point from now, in a certain Dispatcher (thread)
   overriding
   procedure Schedule (Where : in out Dispatcher;
                       What  : Runnable'Class;
                       Time  : Ada.Calendar.Time := Ada.Calendar.Clock)
     with Pre => Time <= Ada.Calendar.Clock or
                 raise Constraint_Error with "Future scheduling in immediate scheduler not allowed";

private

   type Dispatcher is limited new Dispatchers.Dispatcher with null record;

end Rx.Dispatchers.Immediate;
