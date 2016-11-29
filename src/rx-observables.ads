with Ada.Exceptions;

with Rx.Actions;
with Rx.Collections;
with Rx.Errors;
with Rx.Op.Count;
with Rx.Op.Repeat;
with Rx.Preservers;
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

private with Rx.Op.Buffer;
private with Rx.Op.Filter;
private with Rx.Op.Last;
private with Rx.Op.Limit;
private with Rx.Op.No_Op;
private with Rx.Op.Observe_On;
private with Rx.Op.Print;
private with Rx.Op.Subscribe_On;
private with Rx.Src.Empty;
private with Rx.Src.Just;
private with Rx.Src.Start;
private with Rx.Src.Timer;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observables is

   package Typedd renames Typed; -- Bug workaround

   -- Shortcuts
   subtype Observable  is Typed.Contracts.Observable'Class;
   subtype Observer    is Typed.Contracts.Observer'Class;
   subtype Sink        is Typed.Contracts.Sink'Class;
   subtype T           is Typed.Type_Traits.T;

   subtype Definite_Observable is Typed.Definite_Observables.Observable;

   subtype Subscription is Subscriptions.Subscription;

   -- Collections Scaffolding

   package Collections       is new Rx.Collections (Typed);
   package Typed_Lists       renames Collections.Typed_Lists;
   package List_Preservers   renames Collections.List_Preservers;
   package List_Transformers renames Collections.List_Transformers;
   package Obs_Transformers  renames Collections.Obs_Transformers;

   subtype List_Preserver    is List_Preservers.Preserver'Class;
   subtype List_Transformer  is List_Transformers.Transformer'Class;
   subtype Obs_Transformer   is Obs_Transformers.Transformer'Class;
   subtype T_List            is Collections.List;

   -- Preservers Scaffolding

   package Operate   is new Rx.Preservers (Typed);
   subtype Operator  is Operate.Preserver'Class;

   ------------
   -- Buffer --
   ------------

   function Buffer (Every : Positive; Skip : Natural := 0) return List_Transformer;

   -----------
   -- Count --
   -----------

   generic
      with function Succ (V : T) return T is <>;
      Default_Initial_Count : T;
   package Counters is
      package List_Count is new Rx.Op.Count (Collections.List_Transformers_Reverse, Succ, Default_Initial_Count);
      package Self_Count is new Rx.Op.Count (Operate.Transform, Succ, Default_Initial_Count);

      function Count (First : T := Default_Initial_Count) return Operate.Transform.Transformer'Class
                      renames Self_Count.Count;

      function Count (First : T := Default_Initial_Count)
                      return Collections.List_Transformers_Reverse.Transformer'Class
                      renames List_Count.Count;
      --  This counts the number of lists seen, don't confuse with Length

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

      --------------
      -- Interval --
      --------------

      function Interval (First       : Typed.T;
                         Pause       : Duration := 1.0;
                         First_Pause : Duration := 1.0;
                         Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                         return Typed.Observable renames RxInterval.Create;

      -----------------
      -- Range_Count --
      -----------------

      function Range_Count (First : Typed.T;
                            Count : Natural) return Typed.Observable renames RxRange.From_Count;

      -----------------
      -- Range_Slice --
      -----------------

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

   ----------
   -- Last --
   ----------

   function Last return Operator;
   function Last (Check : Typed.Actions.Filter1) return Operator;
   function Last (Check : Typed.Actions.TFilter1'Class) return Operator;

   function Last_Or_Default (V : Typed.T) return Operator;
   function Last_Or_Default (V : Typed.T; Check : Typed.Actions.Filter1) return Operator;
   function Last_Or_Default (V : Typed.T; Check : Typed.Actions.TFilter1'Class) return Operator;

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

   ------------
   -- Repeat --
   ------------

   package RxRepeat is new Rx.Op.Repeat (Operate);

   function Repeat (Times : Positive) return Operator renames RxRepeat.Repeat;

   function Repeat_Forever return Operator renames RxRepeat.Repeat_Forever;

   function Repeat_Until (Check : Actions.TFilter0'Class) return Operator renames RxRepeat.Repeat_Until;

   function Repeat_Until (Check : Actions.Filter0) return Operator is
     (RxRepeat.Repeat_Until (Actions.Wrap (Check)));

   -----------
   -- Start --
   -----------

   function Start (Func :          Typed.Actions.TFunc0'Class) return Observable;

   function Start (Func : not null Typed.Actions.Func0)        return Observable;

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

   function Subscribe (On_Next      : Collections.Typed_Lists.Actions.Proc1 := null;
                       On_Completed : Rx.Actions.Proc0                      := null;
                       On_Error     : Rx.Actions.Proc_Error                 := null)
                       return Collections.Typed_Lists.Sink;

   ------------------
   -- Subscribe_On --
   ------------------

   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operator;

   ----------
   -- Take --
   ----------

   function Take  (Max : Natural) return Operator renames Limit;

   -----------
   -- Timer --
   -----------

   function Timer (V         : T;
                   After     : Duration;
                   Scheduler : Schedulers.Scheduler := Schedulers.Computation)
                   return Observable;


   --------------
   -- While_Do --
   --------------

   function While_Do (Check : Actions.TFilter0'Class) return Operator renames RxRepeat.While_Do;

   function While_Do (Check : Actions.Filter0) return Operator is
      (RxRepeat.While_Do (Actions.Wrap (Check)));

   ----------
   -- Wrap --
   ----------

   function Wrap (Obs : Typed.Observable) return Definite_Observable renames Typed.Definite_Observables.From;
   -- Definite observable
   function "+"  (Obs : Typed.Observable) return Definite_Observable renames Wrap;

   ---------
   -- "&" --
   ---------

   function "&" (Producer : Observable; Consumer : Sink) return Subscriptions.Subscription
                 renames Typed.Contracts.Subscribe;
   --  Final subscription for T observers

   function "&" (Producer : Typed_Lists.Observable; Consumer : Typed_Lists.Sink)
                 return Subscriptions.Subscription
                 renames Typed_Lists.Contracts.Subscribe;
   --  Final subscription for observers of T lists

   function "&" (Producer : Observable; Consumer : Operate.Transform.Transformer'Class) return Observable
                 renames Operate.Transform.Will_Observe;
   --  Concatenation for type preservers

   function "&" (Producer : Observable;
                 Consumer : List_Transformer) return List_Transformers.Intoo.Observable
                 renames List_Transformers.Will_Observe;
   --  Concatenation for groupers into lists

   function "&" (Producer : Collections.List_Transformers_Reverse.Observable;
                 Consumer : Collections.List_Transformers_Reverse.Operator) return Observable
                 renames Collections.List_Transformers_Reverse.Will_Observe;
   --  Concatenation of ungroupers

   function "&" (Producer : List_Preservers.Observable;
                 Consumer : List_Preservers.Transform.Transformer'Class)
                 return     List_Preservers.Observable
                 renames List_Preservers.Transform.Will_Observe;
   --  Concatenation for preservers between lists

   package Linkers is

      --  This package can be used instead of using the Rx.Observables one to make the "&" visible

      function "&" (Producer : Observable; Consumer : Operate.Transform.Transformer'Class) return Observable
                    renames Observables."&";

      function "&" (Producer : Observable; Consumer : Sink) return Subscriptions.Subscription
                    renames Observables."&";

      function "&" (Producer : List_Preservers.Observable;
                    Consumer : List_Preservers.Transform.Transformer'Class)
                    return     List_Preservers.Observable
                    renames List_Preservers.Transform.Will_Observe;

      function "&" (Producer : Collections.List_Transformers_Reverse.Observable;
                    Consumer : Collections.List_Transformers_Reverse.Operator) return Observable
                    renames Collections.List_Transformers_Reverse.Will_Observe;

   end Linkers;


   -- Debug helpers
   function "-" (O : Observable) return Subscriptions.No_Subscription is (null record);

private

   procedure Append (L : in out Collections.List; V : T);

   package RxBuffer is new Rx.Op.Buffer (Collections.List_Transformers,
                                         Collections.Lists.Empty_List);

   function Buffer (Every : Positive; Skip : Natural := 0) return List_Transformers.Transformer'Class
                    renames RxBuffer.Create;

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

   package RxLast is new Rx.Op.Last (Operate);

   function Last return Operator is (RxLast.Create);
   function Last (Check : Typed.Actions.Filter1) return Operator is
     (RxLast.Create (Typed.Actions.Wrap (Check)));
   function Last (Check : Typed.Actions.TFilter1'Class) return Operator is (RxLast.Create (Check));

   function Last_Or_Default (V : Typed.T) return Operator is (RxLast.Or_Default (V));
   function Last_Or_Default (V : Typed.T; Check : Typed.Actions.Filter1) return Operator is
      (RxLast.Or_Default (V, Typed.Actions.Wrap (Check)));
   function Last_Or_Default (V : Typed.T; Check : Typed.Actions.TFilter1'Class) return Operator is
      (RxLast.Or_Default (V, Check));

   package RxLimit is new Rx.Op.Limit (Operate);
   function Limit (Max : Natural) return Operator renames RxLimit.Create;

   package RxNoop is new Rx.Op.No_Op (Operate);
   function No_Op return Operator renames RxNoop.Create;

   package RxObserveOn is new Rx.Op.Observe_On (Operate);
   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxObserveOn.Create;

   package RxPrint is new Rx.Op.Print (Operate);
   function Print (Func           : Typed.Actions.Func1Str := null;
                   With_Timestamp : Boolean                := True) return Operator renames RxPrint.Create;

   package RxStart is new Rx.Src.Start (Typed);
   function Start (Func :          Typed.Actions.TFunc0'Class) return Observable renames RxStart.Create;
   function Start (Func : not null Typed.Actions.Func0)        return Observable
     is (Start (Typed.Actions.Wrap (Func)));

   function Subscribe (On_Next      : Typed.Actions.Proc1   := null;
                       On_Completed : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null) return Sink renames RxSubscribe.Create;

   package RxSubscribeLists is new Rx.Subscribe (Collections.Typed_Lists);
   function Subscribe (On_Next      : Collections.Typed_Lists.Actions.Proc1 := null;
                       On_Completed : Rx.Actions.Proc0                      := null;
                       On_Error     : Rx.Actions.Proc_Error                 := null)
                       return Collections.Typed_Lists.Sink renames RxSubscribeLists.Create;

   package RxSubsOn is new Rx.Op.Subscribe_On (Operate);
   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxSubsOn.Create;

   package RxTimer is new Rx.Src.Timer (Typed);
   function Timer (V         : T;
                   After     : Duration;
                   Scheduler : Schedulers.Scheduler := Schedulers.Computation)
                   return Observable renames RxTimer.Create;

end Rx.Observables;
