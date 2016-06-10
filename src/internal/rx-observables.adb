with Rx.Count;
with Rx.Just;
with Rx.Observe_On;
with Rx.Subscribe;

package body Rx.Observables is

   -----------
   -- Count --
   -----------

   function Count (First : T) return Operator is
      package Self_Count is new Rx.Count (Operate.Transform, Succ); -- Will fail on accessibility check
   begin
      return Self_Count.Count (First);
   end Count;

   ----------
   -- Just --
   ----------

   package RxJust is new Rx.Just (Typed);

   function Just (V : Typed.Type_Traits.T) return Typed.Producers.Observable'Class is
   begin
      return RxJust.Create (V);
   end Just;

   ----------------
   -- Observe_On --
   ----------------

   package RxObserveOn is new Rx.Observe_On (Operate);

   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator is
   begin
      return RxObserveOn.Create (Scheduler);
   end Observe_On;

   ---------------
   -- Subscribe --
   ---------------

   package RxSubscribe is new Rx.Subscribe (Typed);

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Observer is
   begin
      return RxSubscribe.As (On_Next);
   end Subscribe;

   ---------
   -- "&" --
   ---------

   function "&" (L : Typed.Producers.Observable'Class; R : Typed.Consumers.Observer'Class)
                 return Subscriptions.Subscription
   is
      Actual_L : Typed.Producers.Observable'Class := L;
      Actual_R : Typed.Consumers.Observer'Class   := R;
   begin
      Actual_L.Subscribe (Actual_R);
      return Subscriptions.Subscription'(null record);
   end "&";

end Rx.Observables;
