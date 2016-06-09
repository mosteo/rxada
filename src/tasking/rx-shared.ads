with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Shared is

   --  In essence this is a ref counting carcass for a held observer
   type Detached_Observable is new Typed.Mutator with private;

private

   type Detached_Observable is new Typed.Mutator with record
      Actual : access Typed.Consumers.Observer'Class;

      --  We need a queue of pending calls, that's shared and protected? no, that has to be on the scheduler
      --  Basicly, the Observe_On observable stores a pointer to a Scheduler, that can't go out of scope,
      --  and relays values to the Scheduler together with a shared pointer to an observer
   end record;

end Rx.Shared;
