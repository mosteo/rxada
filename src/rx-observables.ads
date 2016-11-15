with Ada.Exceptions;

with Rx.Actions;
with Rx.Errors;
with Rx.Op.Count;
with Rx.Operate;
with Rx.Schedulers;
with Rx.Src.Create;
with Rx.Src.Defer;
with Rx.Src.From;
with Rx.Src.Interval;
with Rx.Src.Ranges;
with Rx.Subscribe;
with Rx.Subscriptions;
with Rx.Traits.Arrays;
with Rx.Typed;

private with Rx.Op.Filter;
private with Rx.Op.Limit;
private with Rx.Op.No_Op;
private with Rx.Op.Observe_On;
private with Rx.Op.Print;
private with Rx.Op.Subscribe_On;
private with Rx.Src.Empty;
private with Rx.Src.Just;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observables is

   package Typedd renames Typed; -- Bug workaround

   -- Shortcuts
   subtype Sink        is Typed.Contracts.Sink'Class;
   subtype Observable  is Typed.Contracts.Observable'Class;
   subtype Observer    is Typed.Contracts.Observer'Class;
   subtype T           is Typed.Type_Traits.T;
   subtype Defob       is Typed.Defobs.Observable;

   -- Scaffolding
   package Operate is new Rx.Operate (Typed);
   subtype Operator is Operate.Operator'Class;

   -----------
   -- Count --
   -----------

   generic
      with function Succ (V : T) return T is <>;
      Default_Initial_Count : T;
   package Counters is
      package Self_Count is new Rx.Op.Count (Operate.Transform, Succ, Default_Initial_Count);

      function Count (First : T := Default_Initial_Count) return Operate.Transform.Operator'Class
                      renames Self_Count.Count;
   end Counters;

   -----------
   -- Enums --
   -----------

   generic
      with function Succ (V : T)    return T       is <>;
      with function "<"  (L, R : T) return Boolean is <>;
   package Enums is

      package RxInterval is new Rx.Src.Interval (Typed, Succ);
      package RxRange    is new Rx.Src.Ranges   (Typed, Succ, "<");

      function Interval (First       : Typed.T;
                         Pause       : Duration := 1.0;
                         First_Pause : Duration := 1.0;
                         Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                         return Typed.Observable renames RxInterval.Create;

      function Range_Count (First : Typed.T;
                            Count : Natural) return Typed.Observable renames RxRange.From_Count;

      function Range_Slice (First : Typed.T;
                            Last  : Typed.T) return Typed.Observable renames RxRange.From_Slice;

   end Enums;

   ------------
   -- Create --
   ------------

   package RxCreate is new Rx.Src.Create (Typed);

   function Create (On_Subscribe : not null access procedure (Observer : in out Typed.Subscriber))
                    return Typed.Observable renames RxCreate.Parameterless;

   function Create (Observable : RxCreate.Observable'Class) return Typed.Observable
                    renames RxCreate.Tagged_Stateful;

   -----------
   -- Defer --
   -----------

   package RxDefer is new Rx.Src.Defer (Typed);

   function Defer (Factory : RxDefer.Factory'Class) return Typed.Observable renames RxDefer.Create;

   function Defer (Factory : RxDefer.Factory_Func) return Typed.Observable renames RxDefer.Create;

   -----------
   -- Empty --
   -----------

   function Empty return Typed.Observable;

   -----------
   -- Error --
   -----------

   function Error (E : Rx.Errors.Occurrence)                return Typed.Observable;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Typed.Observable;

   ------------
   -- Filter --
   ------------

   function Filter (Check : not null Typed.Actions.Filter1) return Operator;

   ----------
   -- From --
   ----------

   package Default_Arrays is new Rx.Traits.Arrays (Typed, Integer);
   package Arrays renames Default_Arrays;

   -- Observable from an array of values, useful for literal arrays
   function From (A : Default_Arrays.Typed_Array) return Observable;

   ----------
   -- Just --
   ----------

   -- Observable from single value
   function Just (V : T) return Observable;

   -----------
   -- Limit --
   -----------

   function Limit (Max : Natural) return Operator;

   -----------
   -- Never --
   -----------

   function Never return Typed.Observable;

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
                       On_Error     : Rx.Actions.Proc_Error := null) return Sink;

   procedure Subscribe (Producer     : Typed.Observable;
                        On_Next      : Typed.Actions.Proc1   := null;
                        On_Completed : Rx.Actions.Proc0      := null;
                        On_Error     : Rx.Actions.Proc_Error := null);

   package RxSubscribe is new Rx.Subscribe (Typed);
   type Subscriptor is abstract new RxSubscribe.Subscribe with null record;
   --  You can alternatively override methods of this type to more easily provide context

   ------------------
   -- Subscribe_On --
   ------------------

   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operator;

   ----------
   -- Take --
   ----------

   function Take  (Max : Natural) return Operator renames Limit;

   ----------
   -- Wrap --
   ----------

   function Wrap (Obs : Typed.Observable) return Defob renames Typed.Defobs.From;
   -- Definite observable
   function "+"  (Obs : Typed.Observable) return Defob renames Wrap;

   ---------
   -- "&" --
   ---------

   --  Chain preparation

   function "&" (Producer : Observable; Consumer : Operate.Transform.Operator'Class) return Observable
   renames Operate.Transform.Will_Observe;

   --  Subscribe
   function "&" (Producer : Observable; Consumer : Sink) return Subscriptions.Subscription;

   -- Debug helpers
   function "-" (O : Observable) return Subscriptions.No_Subscription is (null record);

private

   package RxEmpty is new Rx.Src.Empty (Typed);
   function Empty return Typed.Observable renames RxEmpty.Empty;
   function Never return Typed.Observable renames RxEmpty.Never;

   function Error (E : Rx.Errors.Occurrence)                return Typed.Observable renames RxEmpty.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Typed.Observable renames RxEmpty.Error;

   package RxFilter is new Rx.Op.Filter (Operate);
   function Filter (Check : not null Typed.Actions.Filter1) return Operator renames RxFilter.Create;

   package From_Arrays is new Rx.Src.From.From_Array (Default_Arrays);
   function From (A : Default_Arrays.Typed_Array) return Observable
                  renames From_Arrays.From;

   package RxJust is new Rx.Src.Just (Typed);
   function Just (V : T) return Observable renames RxJust.Create;

   package RxLimit is new Rx.Op.Limit (Operate);
   function Limit (Max : Natural) return Operator renames RxLimit.Create;

   package RxNoop is new Rx.Op.No_Op (Operate);
   function No_Op return Operator renames RxNoop.Create;

   package RxObserveOn is new Rx.Op.Observe_On (Operate);
   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxObserveOn.Create;

   package RxPrint is new Rx.Op.Print (Operate);
   function Print (Func           : Typed.Actions.Func1Str := null;
                   With_Timestamp : Boolean                := True) return Operator renames RxPrint.Create;

   function Subscribe (On_Next      : Typed.Actions.Proc1   := null;
                       On_Completed : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null) return Sink renames RxSubscribe.Create;

   package RxSubsOn is new Rx.Op.Subscribe_On (Operate);
   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxSubsOn.Create;

end Rx.Observables;
