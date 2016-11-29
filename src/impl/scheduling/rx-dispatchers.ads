with Ada.Calendar;

with Rx.Errors;
with Rx.Impl.Shared_Subscriber;
with Rx.Preservers;
with Rx.Typed;

package Rx.Dispatchers is

   pragma Elaborate_Body;

   type Dispatcher is limited interface;

   type Runnable is interface;

   procedure Run (This : in out Runnable) is abstract;

   --  Schedule a code to be run at a certain time, in a certain scheduler (thread)
   procedure Schedule (Where : in out Dispatcher;
                       What  : in out Runnable'Class;
                       Time  : Ada.Calendar.Time := Ada.Calendar.Clock) is abstract;

   generic
      with package Typed is new Rx.Typed (<>);
   package Events is

      package Shared is new Rx.Impl.Shared_Subscriber (Typed);

      procedure On_Next      (Sched : in out Dispatcher'Class; Observer : Shared.Subscriber; V : Typed.Type_Traits.T);
      procedure On_Completed (Sched : in out Dispatcher'Class; Observer : Shared.Subscriber);
      procedure On_Error     (Sched : in out Dispatcher'Class; Observer : Shared.Subscriber; E : Rx.Errors.Occurrence);

   end Events;

   generic
      with package Operate is new Rx.Preservers (<>);
   package Subscribe is

      procedure On_Subscribe (Sched : in out Dispatcher'Class; Operator : Operate.Operator'Class);

   end Subscribe;

   procedure Shutdown;
   --  Signal schedulers to exit.
   --  Necessary when there are infinite sequences going on (e.g. Interval)

   function Terminating return Boolean;
   --  Will be true after shutdown has been invoked

end Rx.Dispatchers;
