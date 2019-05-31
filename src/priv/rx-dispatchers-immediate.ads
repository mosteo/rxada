package Rx.Dispatchers.Immediate is

   use type Ada.Calendar.Time;

   type Dispatcher is limited new Dispatchers.Dispatcher with private;

   --  Schedule a code to be run at a certain point from now, in a certain Dispatcher (thread)
   overriding
   procedure Schedule (Where : in out Dispatcher;
                       What  : Runnable'Class;
                       Time  : Ada.Calendar.Time := Ada.Calendar.Clock);
   --  Providing a future time in this scheduler will result in Constraint_Error

private

   type Dispatcher is limited new Dispatchers.Dispatcher with null record;

end Rx.Dispatchers.Immediate;
