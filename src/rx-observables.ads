	with Rx.Actions;
	with Rx.Op.Count;
private with Rx.Op.No_Op;
	with Rx.Src.From;
private with Rx.Src.Just;
private with Rx.Op.Observe_On;
	with Rx.Operate;
private with Rx.Op.Print;
	with Rx.Schedulers;
private with Rx.Subscribe;
private with Rx.Subscribe_On;
	with Rx.Subscriptions;
	with Rx.Traits.Arrays;
	with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observables is

   -- Shortcuts
   subtype Observable is Typed.Producers.Observable'Class;
   subtype Observer   is Typed.Consumers.Observer'Class;
   subtype T is Typed.Type_Traits.T;

   -- Scaffolding
   package Operate is new Rx.Operate (Typed);
   subtype Operator is Operate.Operator'Class;

   -----------
   -- Count --
   -----------

   generic
      with function Succ (V : T) return T;
   package Counters is
      package Self_Count is new Rx.Count (Operate.Transform, Succ);

      function Count (First : T) return Operator renames Self_Count.Count;
   end Counters;

   ----------
   -- From --
   ----------

   package Default_Arrays is new Rx.Traits.Arrays (Typed, Integer);

   -- Observable from an array of values, useful for literal arrays
   function From (A : Default_Arrays.Typed_Array) return Observable;

   ----------
   -- Just --
   ----------

   -- Observable from single value
   function Just (V : T) return Observable;

   -----------
   -- No_Op --
   -----------

   function No_Op return Operator;

   ----------------
   -- Observe_On --
   ----------------

   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator;

   -----------
   -- Print --
   -----------

   function Print (Func : Typed.Actions.Func1Str := null; With_Timestamp : Boolean := True) return Operator;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (On_Next      : Typed.Actions.Proc1   := null;
                       On_Completed : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null) return Observer;

   ------------------
   -- Subscribe_On --
   ------------------

   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operate.Operator;

   ---------
   -- "&" --
   ---------

   --  Chain preparation
   function "&" (L : Observable;
                 R : Operator)
                 return Observable renames Operate.Transform."&"; -- OMG

   --  Subscribe
   function "&" (L : Observable;
                 R : Observer)
                 return Subscriptions.No_Subscription;

private

   package From_Arrays is new Rx.From.From_Array (Default_Arrays);
   function From (A : Default_Arrays.Typed_Array) return Observable
                  renames From_Arrays.From;

   package RxJust is new Rx.Just (Typed);
   function Just (V : T) return Observable renames RxJust.Create;

   package RxNoop is new Rx.Op.No_Op (Operate);
   function No_Op return Operator renames RxNoop.Create;

   package RxObserveOn is new Rx.Observe_On (Operate);
   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxObserveOn.Create;

   package RxPrint is new Rx.Print (Operate);
   function Print (Func           : Typed.Actions.Func1Str := null;
                   With_Timestamp : Boolean                := True) return Operator renames RxPrint.Create;

   package RxSubscribe is new Rx.Subscribe (Typed);
   function Subscribe (On_Next      : Typed.Actions.Proc1   := null;
                       On_Completed : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null) return Observer renames RxSubscribe.Create;

   package RxSubsOn is new Rx.Subscribe_On (Operate);
   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operate.Operator renames RxSubsOn.Create;

end Rx.Observables;
