with Rx.Errors;
with Rx.Shared;
with Rx.Typed;

package Rx.Scheduler is

   type Object is limited interface;

   type Runnable is interface;

   procedure Run (This : in out Runnable) is abstract;

   --  Schedule a code to be run at a certain point from now, in a certain scheduler (thread)
   procedure Schedule (Where : in out Object; What : Runnable'Class; After : Duration := 0.0) is abstract;

   generic
      with package Typed is new Rx.Typed (<>);
   package Events is

      package Shared is new Rx.Shared (Typed);

      procedure On_Next      (Sched : in out Object'Class; Observer : Shared.Observer; V : Typed.Type_Traits.T);
      procedure On_Completed (Sched : in out Object'Class; Observer : Shared.Observer);
      procedure On_Error     (Sched : in out Object'Class; Observer : Shared.Observer; E : Rx.Errors.Occurrence);

   end Events;

end Rx.Scheduler;
