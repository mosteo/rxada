package Rx.Dispatchers.Immediate is

   type Dispatcher is limited new Dispatchers.Dispatcher with private;

   --  Schedule a code to be run at a certain point from now, in a certain Dispatcher (thread)
   overriding
   procedure Schedule (Where : in out Dispatcher;
                       What  : in out Runnable'Class;
                       Time  : Ada.Calendar.Time := Ada.Calendar.Clock);

private

   type Dispatcher is limited new Dispatchers.Dispatcher with null record;

end Rx.Dispatchers.Immediate;
