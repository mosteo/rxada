with Ada.Real_Time.Timing_Events;

package Rx.Scheduler is

   type Object is limited interface;

   type Runnable is abstract tagged limited private;

   procedure Run (This : in out Runnable) is null;

   --  Schedule a code to be run at a certain point from now, in a certain scheduler (thread)
   procedure Schedule (Where : in out Object; What : in out Runnable'Class; After : Duration := 0.0) is abstract;

private

   type Runnable is abstract new Ada.Real_Time.Timing_Events.Timing_Event with null record;

end Rx.Scheduler;
