with Ada.Calendar;

with Rx.Errors;
with Rx.Operate;
with Rx.Shared;
with Rx.Typed;

package Rx.Dispatchers is

--     pragma Preelaborate;

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

      package Shared is new Rx.Shared (Typed);

      procedure On_Next      (Sched : in out Dispatcher'Class; Observer : Shared.Observer; V : Typed.Type_Traits.T);
      procedure On_Completed (Sched : in out Dispatcher'Class; Observer : Shared.Observer);
      procedure On_Error     (Sched : in out Dispatcher'Class; Observer : Shared.Observer; E : Rx.Errors.Occurrence);

   end Events;

   generic
      with package Operate is new Rx.Operate (<>);
   package Subscribe is

      procedure On_Subscribe (Sched : in out Dispatcher'Class; Operator : Operate.Operator);

   end Subscribe;

end Rx.Dispatchers;
