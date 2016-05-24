package Rx.Scheduler is

   pragma Pure;

   type Object is limited interface;

   type Runnable is interface;

   procedure Run (This : Runnable) is null;

   --  Schedule a code to be run at a certain point from now, in a certain scheduler (thread)
   procedure Schedule (Where : in out Object; What : Runnable'Class; After : Duration := 0.0) is abstract;

end Rx.Scheduler;
