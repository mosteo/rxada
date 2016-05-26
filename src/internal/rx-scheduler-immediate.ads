package Rx.Scheduler.Immediate is

   type Object is limited new Scheduler.Object with private;

   --  Schedule a code to be run at a certain point from now, in a certain scheduler (thread)
   overriding
   procedure Schedule (Where : in out Object; What : in out Runnable'Class; After : Duration := 0.0);

private

   type Object is limited new Scheduler.Object with null record;

end Rx.Scheduler.Immediate;
