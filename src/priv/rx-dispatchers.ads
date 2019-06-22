with Ada.Calendar;

with Rx.Errors;
with Rx.Impl.Shared_Observer;
with Rx.Impl.Preservers;
with Rx.Impl.Typed;

package Rx.Dispatchers is

   pragma Elaborate_Body;

   type Dispatcher is limited interface;

   type Runnable is interface;

   --  Not made explicit, but implementors of this class are (and must be)
   --  synchronized

   procedure Run (This : Runnable) is abstract;

   --  Schedule a code to be run at a certain time, in a certain scheduler (thread)
   procedure Schedule (Where : in out Dispatcher;
                       What  : Runnable'Class;
                       Time  : Ada.Calendar.Time := Ada.Calendar.Clock) is abstract;

   generic
      with package Typed is new Rx.Impl.Typed (<>);
   package Events is

      package Shared is new Rx.Impl.Shared_Observer (Typed);

      procedure On_Next      (Sched : in out Dispatcher'Class; Observer : Shared.Observer; V : Typed.Type_Traits.T);
      procedure On_Complete  (Sched : in out Dispatcher'Class; Observer : Shared.Observer);
      procedure On_Error     (Sched : in out Dispatcher'Class; Observer : Shared.Observer; E : Rx.Errors.Occurrence);

   end Events;

   generic
      with package Operate is new Rx.Impl.Preservers (<>);
   package Subscribe is

      procedure On_Subscribe (Sched  : in out Dispatcher'Class;
                              Parent :        Operate.Observable'Class;
                              Child  :        Operate.Into.Observer'Class);

   end Subscribe;

   procedure Shutdown;
   --  Signal schedulers to exit.
   --  Necessary when there are infinite sequences going on (e.g. Interval)

   function Terminating return Boolean;
   --  Will be true after shutdown has been invoked

end Rx.Dispatchers;
