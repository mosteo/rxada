with Ada.Exceptions;

with Rx.Actions;
with Rx.Collections;
with Rx.Errors;
with Rx.Impl.Preservers;
with Rx.Schedulers;
with Rx.Subscribe;
with Rx.Subscriptions;
with Rx.Traits.Arrays;
with Rx.Impl.Typed;
with Rx.Valueless;

private with Rx.Op.Buffer;
private with Rx.Op.Debounce;
private with Rx.Op.Distinct;
private with Rx.Op.Do_On;
private with Rx.Op.Element_At;
private with Rx.Op.Filter;
private with Rx.Op.Flatmap;
private with Rx.Op.Hold;
private with Rx.Op.Last;
private with Rx.Op.Limit;
private with Rx.Op.Map;
private with Rx.Op.Merge;
private with Rx.Op.No_Op;
private with Rx.Op.Observe_On;
private with Rx.Op.Print;
private with Rx.Op.Repeat;
private with Rx.Op.Sample;
private with Rx.Op.Split;
private with Rx.Op.Stopwatch;
private with Rx.Op.Subscribe_On;
private with Rx.Op.Take;
private with Rx.Src.Create;
private with Rx.Src.Defer;
private with Rx.Src.Empty;
private with Rx.Src.From;
private with Rx.Src.Just;
private with Rx.Src.Start;
private with Rx.Src.Timer;

generic
   with package User_Typed is new Rx.Impl.Typed (<>);
package Rx.Observables is

   package Typed renames User_Typed; -- Bug workaround

   package Contracts renames Typed.Contracts;
   package Factories renames Typed.Factories;

   -- Type Shortcuts
   subtype Observable  is Typed.Contracts.Observable'Class;
   subtype Observer    is Typed.Contracts.Observer'Class;
   subtype Sink        is Typed.Contracts.Sink'Class;
   subtype T           is Typed.Type_Traits.T;

   subtype Definite_Observable is Typed.Definite_Observables.Observable;

   -- Collections Scaffolding

   package Collections is new  Rx.Collections (Typed);

   package Typed_Lists            renames Collections.Typed_Lists;
   package List_Preservers        renames Collections.List_Preservers;
   package Into_List_Transformers renames Collections.Into_List_Transformers;
   package From_List_Transformers renames Collections.From_List_Transformers;
--     package Obs_Transformers       renames Collections.Obs_Transformers;
   package Into_Valueless         renames Collections.Valueless;
--
   subtype List_Preserver         is List_Preservers.Operator'Class;
   subtype Into_List_Transformer  is Into_List_Transformers.Operator'Class;
   subtype From_List_Transformer  is From_List_Transformers.Operator'Class;
--     subtype Obs_Transformer        is Obs_Transformers.Operator'Class;
   subtype T_List                 is Collections.List;

   -- Preservers Scaffolding

   package Operate   is new Rx.Impl.Preservers (Typed);
   subtype Operator  is Operate.Operator'Class;

   ------------
   -- Buffer --
   ------------

   function Buffer (Every : Positive; Skip : Natural := 0) return Into_List_Transformer;

   ------------
   -- Create --
   ------------

   function Create (On_Subscribe : not null access procedure (Observer : in out Typed.Observer))
                    return Observable;

   function Create (Initial : T;
                    Succ    : not null Typed.Actions.Func1;
                    Count   : Rx_Integer := Rx_Integer'Last) return Observable;

   --  You can always extend a plain Contracts.Observable, of course!
   --  Also, see Rx.Op.Create for other alternatives

   --------------
   -- Debounce --
   --------------

   function Debounce (Window : Duration) return Operator;

   -----------
   -- Defer --
   -----------

   function Defer (Factory : Factories.Observable_Factory'Class) return Observable;

   function Defer (Factory : Factories.observable_Factory_Func) return Observable;

   --------------
   -- Distinct --
   --------------

   Default_Not_Same : constant Typed.Actions.Comparator;

   function Distinct (Are_Distinct : Typed.Actions.Comparator := Default_Not_Same) return Operator;

   -----------
   -- Do_On --
   -----------

   function Do_On (Next : Typed.Actions.Proc1) return Operator;
   function Do_On (Next : Typed.Actions.TProc1'Class) return Operator;

   ----------------
   -- Element_At --
   ----------------

   function Element_At (Pos : Rx_Integer; First : Rx_Integer := 1) return Operator;

   function Element_At_Or_Default (Default : T; Pos : Rx_Integer; First : Rx_Integer := 1) return Operator;

   -----------
   -- Empty --
   -----------

   function Empty return Observable;

   -----------
   -- Error --
   -----------

   function Error (E : Rx.Errors.Occurrence)                return Observable;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Observable;

   ------------
   -- Expand --
   ------------

   function Expand (Func     : Operate.Transform.Actions.Inflater1) return Operator;
   function Expand (Func     : Operate.Transform.Actions.TInflater1'Class) return Operator;
   function Expand (Pipeline : Observable) return Operator;
   --  See Flat_Map notes below

   ------------
   -- Filter --
   ------------

   function Filter (Check : not null Typed.Actions.Filter1) return Operator;
   function Filter (Check : Typed.Actions.TFilter1'Class) return Operator;

   --------------
   -- Flat_Map --
   --------------
   function Flat_Map (Func      : Operate.Transform.Actions.Inflater1;
                      Recursive : Boolean := False) return Operator;

   function Flat_Map (Func      : Operate.Transform.Actions.TInflater1'Class;
                      Recursive : Boolean := False) return Operator;

   function Flat_Map (Pipeline  : Observable'Class;
                      Recursive : Boolean := False) return Operator;
   -- NOTE: Pipeline must actually be an operator, but since "&" returns Observables...
   --  See notes in Rx.Op.Flat_Map for more details
   --  Applies Just & Pipeline to incoming values

   --------------
   -- For_Each --
   --------------

   procedure For_Each (Producer     : Typed.Observable;
                       On_Next      : Typed.Actions.Proc1   := null;
                       On_Complete  : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null);

   procedure For_Each (Producer : Typed.Observable;
                       Consumer : Typed.Sink);

   ----------
   -- From --
   ----------

   package Default_Arrays is new Rx.Traits.Arrays (Typed, Rx_Integer);
   package Arrays renames Default_Arrays;

   -- Observable from an array of values, useful for literal arrays
   function From (A : Default_Arrays.Typed_Array) return Observable;

   ----------
   -- Hold --
   ----------

   function Hold (Fixed  : Duration; Random : Duration := 0.0) return Operator;
   --  Busy delay; this hogs the thread it is running on!

   ----------
   -- Just --
   ----------

   -- Observable from single value
   function Just (V : T) return Observable;

   ----------
   -- Last --
   ----------

   function Last return Operator;
   function Last (Check : Typed.Actions.Filter1)        return Operator;
   function Last (Check : Typed.Actions.TFilter1'Class) return Operator;

   function Last_Or_Default (V : T) return Operator;
   function Last_Or_Default (V : T; Check : Typed.Actions.Filter1) return Operator;
   function Last_Or_Default (V : T; Check : Typed.Actions.TFilter1'Class) return Operator;

   -----------
   -- Limit --
   -----------

   function Limit (Max : Rx_Natural) return Operator;

   ---------
   -- Map --
   ---------

   function Map (F : Operate.Transform.Actions.Func1) return Operator;

   --  See the particular "&" below

   -----------
   -- Merge --
   -----------

   function Merge (One, Two : Observable) return Observable;
   --  Merges two observables using the same scheduler

   function Merge_With (Merge_With : Observable) return Operator;
   --  Merges observable in current stream, using scheduler only for the
   --  new observable

   -----------
   -- Never --
   -----------

   function Never return Observable;

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

   function Repeat (Times : Rx_Integer) return Operator;

   function Repeat_Forever return Operator;

   function Repeat_Until (Check : Actions.TFilter0'Class) return Operator;

   function Repeat_Until (Check : Actions.Filter0) return Operator;

   ------------
   -- Sample --
   ------------

   type Policies is (Keep_First, Keep_Last);

   function Sample (Policy  : Policies;
                    Sampler : Valueless.Observable'Class) return Operator;

   -----------
   -- Split --
   -----------

   function Split return From_List_Transformer;

   -----------
   -- Start --
   -----------

   function Start (Func :          Typed.Actions.TFunc0'Class) return Observable;

   function Start (Func : not null Typed.Actions.Func0)        return Observable;

   ---------------
   -- Stopwatch --
   ---------------

   function Stopwatch (Proc : not null Actions.Inspector) return Operator;

   -----------
   -- Strip --
   -----------

   function Strip return Into_Valueless.Operator;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (On_Next      : Typed.Actions.Proc1   := null;
                       On_Complete  : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null) return Sink;

   function Subscribe (Observer : Typed.Observer) return Sink;

   function Subscribe (On_Next      : Collections.Typed_Lists.Actions.Proc1 := null;
                       On_Complete  : Rx.Actions.Proc0                      := null;
                       On_Error     : Rx.Actions.Proc_Error                 := null)
                       return Collections.Typed_Lists.Sink;

   procedure Subscribe (Producer     : Observable;
                        On_Next      : Typed.Actions.Proc1   := null;
                        On_Complete  : Rx.Actions.Proc0      := null;
                        On_Error     : Rx.Actions.Proc_Error := null) renames For_Each;

   ------------------
   -- Subscribe_On --
   ------------------

   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operator;

   ----------
   -- Take --
   ----------

   function Take  (Count : Rx_Natural) return Operator;

   function Take_Until (Done : not null Typed.Actions.Filter1)        return Operator;
   function Take_Until (Done :          Typed.Actions.TFilter1'Class) return Operator;

   function Take_While (Pass : not null Typed.Actions.Filter1)        return Operator;
   function Take_While (Pass :          Typed.Actions.TFilter1'Class) return Operator;

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

   function While_Do (Check : Actions.TFilter0'Class) return Operator;

   function While_Do (Check : Actions.Filter0) return Operator;

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

   function "&" (Producer : Observable; Consumer : Operator) return Observable
                 renames Operate.Concatenate;
   --  Concatenation for type preservers

   function "&" (Producer : Observable;
                 Consumer : Into_List_Transformer)
                 return     Into_List_Transformers.Into_Observable
                 renames Into_List_Transformers.Concatenate;
   --  Concatenation for groupers into lists

   function "&" (Producer : From_List_Transformers.From_Observable;
                 Consumer : From_List_Transformer) return Observable
                 renames From_List_Transformers.Concatenate;
   --  Concatenation of ungroupers

   function "&" (Producer : List_Preservers.Observable;
                 Consumer : List_Preservers.Operator'Class)
                 return     List_Preservers.Observable
                 renames List_Preservers.Concatenate;
   --  Concatenation for preservers between lists

   function "&" (Producer : Observable'Class;
                 Consumer : Into_Valueless.Operator'Class) return Into_Valueless.Into_Observable'Class
                 renames Into_Valueless.Concatenate;
   --  Concatenation for ??

   function "&" (Producer : Observable'Class;
                 Consumer : Operate.Transform.Actions.Func1)
                 return Observable;
   --  Implicit Map

   function "&" (Producer : Operate.Observable'Class;
                 Consumer : Operate.Transform.Actions.Inflater1)
                 return Operate.Observable'Class;
   --  Implicit Flat_Map

   function "&" (Producer : Operate.Observable'Class;
                 Consumer : Operate.Transform.Actions.TInflater1'Class)
                 return Operate.Observable'Class;
   --  Implicit Flat_Map

   package Linkers is

      --  This package can be used instead of using the Rx.Observables one to make the "&" visible

      function "&" (Producer : Observable; Consumer : Operate.Operator'Class) return Observable
                    renames Observables."&";

      function "&" (Producer : Observable; Consumer : Sink) return Subscriptions.Subscription
                    renames Observables."&";

      function "&" (Producer : List_Preservers.Observable;
                    Consumer : List_Preservers.Operator'Class)
                    return     List_Preservers.Observable
                    renames List_Preservers.Concatenate;

      function "&" (Producer : From_List_Transformers.From_Observable;
                    Consumer : From_List_Transformer) return Observable
                    renames From_List_Transformers.Concatenate;

      function "&" (Producer : Observable'Class;
                    Consumer : Into_Valueless.Operator'Class) return Into_Valueless.Into_Observable'Class
                    renames Into_Valueless.Concatenate;

      function "&" (Producer : Observable'Class;
                    Consumer : Operate.Transform.Actions.Func1)
                    return Observable renames Observables."&";

      function "&" (Producer : Operate.Observable'Class;
                    Consumer : Operate.Transform.Actions.Inflater1)
                    return Operate.Observable'Class renames Observables."&";

      function "&" (Producer : Operate.Observable'Class;
                    Consumer : Operate.Transform.Actions.TInflater1'Class)
                 return Operate.Observable'Class renames Observables."&";

   end Linkers;


   -- Debug helpers
   function "-" (Dummy : Observable) return Subscriptions.No_Subscription is (null record);

private

   procedure Append (L : in out Collections.List; V : T);

   procedure Set_Parent (This   : in out Observable'Class;
                         Parent :        Observable'Class);

   --------------
   -- RxBuffer --
   --------------

   package RxBuffer is new Rx.Op.Buffer (Into_List_Transformers,
                                         Collections.Lists.Empty_List);
   function Buffer (Every : Positive; Skip : Natural := 0) return Into_List_Transformer
                    renames RxBuffer.Create;

   --------------
   -- RxCreate --
   --------------

   package RxCreate is new Rx.Src.Create (Typed);
   function Create (On_Subscribe : not null access procedure (Observer : in out Typed.Observer))
                    return Observable renames RxCreate.Parameterless;
   function Create (Initial : T;
                    Succ    : not null Typed.Actions.Func1;
                    Count   : Rx_Integer := Rx_Integer'Last) return Observable renames RxCreate.Enumerator;

   ----------------
   -- RxDebounce --
   ----------------

   package RxDebounce is new Op.Debounce (Operate);
   function Debounce (Window : Duration) return Operator renames RxDebounce.Create;

   -------------
   -- RxDefer --
   -------------

   package RxDefer is new Rx.Src.Defer (Typed);
   function Defer (Factory : Factories.Observable_Factory'Class) return Observable renames RxDefer.Create;
   function Defer (Factory : Factories.Observable_Factory_Func) return Observable renames RxDefer.Create;

   ----------------
   -- RxDistinct --
   ----------------

   package RxDistinct is new Rx.Op.Distinct (Operate);
   Default_Not_Same : constant Typed.Actions.Comparator := RxDistinct.Default_Not_Same'Access;
   function Distinct (Are_Distinct : Typed.Actions.Comparator := Default_Not_Same) return Operator
                      renames RxDistinct.Create;

   ----------
   -- RxDo --
   ----------

   package RxDo is new Rx.Op.Do_On (Operate);
   function Do_On (Next : Typed.Actions.TProc1'Class) return Operator renames RxDo.Create;
   function Do_On (Next : Typed.Actions.Proc1) return Operator is
     (Do_On (Typed.Actions.Wrap (Next)));

   -----------------
   -- RxElementAt --
   -----------------

   package RxElementAt is new Rx.Op.Element_At (Operate);
   function Element_At (Pos : Rx_Integer; First : Rx_Integer := 1) return Operator renames RxElementAt.Create;
   function Element_At_Or_Default (Default : T; Pos : Rx_Integer; First : Rx_Integer := 1) return Operator
                                   renames RxElementAt.Or_Default;

   -------------
   -- RxEmpty --
   -------------

   package RxEmpty is new Rx.Src.Empty (Typed);
   function Empty return Observable renames RxEmpty.Empty;
   function Never return Observable renames RxEmpty.Never;

   function Error (E : Rx.Errors.Occurrence)                return Observable renames RxEmpty.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Observable renames RxEmpty.Error;

   ------------
   -- Expand --
   ------------

   function Expand (Func     : Operate.Transform.Actions.Inflater1) return Operator is
      (Flat_Map (Func, Recursive => True));
   function Expand (Func     : Operate.Transform.Actions.TInflater1'Class) return Operator is
      (Flat_Map (Func, Recursive => True));
   function Expand (Pipeline : Observable) return Operator is
      (Flat_Map (Pipeline, Recursive => True));

   --------------
   -- RxFilter --
   --------------

   package RxFilter is new Rx.Op.Filter (Operate);
   function Filter (Check : not null Typed.Actions.Filter1) return Operator renames RxFilter.Create;
   function Filter (Check : Typed.Actions.TFilter1'Class) return Operator renames RxFilter.Create;

   ---------------
   -- RxFlatMap --
   ---------------

   function Identity (This : Operate.From.Observer'Class) return Operate.Into.Observer'Class is (This);

   package RxFlatMap is new Rx.Op.Flatmap (Operate.Transform,
                                           Identity, Operate.Identity,
                                           Set_Parent);
   function Flat_Map (Func      : Operate.Transform.Actions.Inflater1;
                      Recursive : Boolean := False) return Operator renames RxFlatMap.Create;
   function Flat_Map (Func      : Operate.Transform.Actions.TInflater1'Class;
                      Recursive : Boolean := False) return Operator renames RxFlatMap.Create;
   function Flat_Map (Pipeline  : Observable'Class;
                      Recursive : Boolean := False) return Operator renames RxFlatMap.Create;
   function "&" (Producer : Operate.Observable'Class;
                 Consumer : Operate.Transform.Actions.Inflater1)
                 return Operate.Observable'Class renames RxFlatMap."&";
   function "&" (Producer : Operate.Observable'Class;
                 Consumer : Operate.Transform.Actions.TInflater1'Class)
                 return Operate.Observable'Class renames RxFlatMap."&";

   -----------------
   -- From_Arrays --
   -----------------

   package From_Arrays is new Rx.Src.From.From_Array (Default_Arrays);
   function From (A : Default_Arrays.Typed_Array) return Observable
                  renames From_Arrays.From;

   ------------
   -- RxHold --
   ------------

   package RxHold is new Rx.Op.Hold (Operate);
   function Hold (Fixed  : Duration; Random : Duration := 0.0) return Operator
     renames RxHold.Create;

   ------------
   -- RxJust --
   ------------

   package RxJust is new Rx.Src.Just (Typed);
   function Just (V : T) return Observable renames RxJust.Create;

   ------------
   -- RxLast --
   ------------

   package RxLast is new Rx.Op.Last (Operate);

   ----------
   -- Last --
   ----------

   function Last return Operator is (RxLast.Create);
   function Last (Check : Typed.Actions.Filter1) return Operator is
     (RxLast.Create (Typed.Actions.Wrap (Check)));
   function Last (Check : Typed.Actions.TFilter1'Class) return Operator is (RxLast.Create (Check));

   function Last_Or_Default (V : T) return Operator is (RxLast.Or_Default (V));
   function Last_Or_Default (V : T; Check : Typed.Actions.Filter1) return Operator is
      (RxLast.Or_Default (V, Typed.Actions.Wrap (Check)));
   function Last_Or_Default (V : T; Check : Typed.Actions.TFilter1'Class) return Operator is
      (RxLast.Or_Default (V, Check));

   -------------
   -- RxLimit --
   -------------

   package RxLimit is new Rx.Op.Limit (Operate);
   function Limit (Max : Rx_Natural) return Operator renames RxLimit.Create;

   -----------
   -- RxMap --
   -----------

   package RxMap is new Rx.Op.Map (Operate.Transform);
   function Map (F : Operate.Transform.Actions.Func1) return Operator renames RxMap.Create;
   function "&" (Producer : Observable'Class;
                 Consumer : Operate.Transform.Actions.Func1) return Observable renames RxMap."&";

   -------------
   -- RxMerge --
   -------------

   package RxMerge is new Rx.Op.Merge (Operate);
   function Merge_With (Merge_With : Observable) return Operator is
     (RxMerge.Create (Merge_With, Rx.Merge));

   -----------
   -- Merge --
   -----------

   function Merge (One, Two : Observable) return Observable is
     (RxMerge.Create (One, Two, Rx.Merge));

   ------------
   -- RxNoop --
   ------------

   package RxNoop is new Rx.Op.No_Op (Operate);
   function No_Op return Operator renames RxNoop.Create;

   -----------------
   -- RxObserveOn --
   -----------------

   package RxObserveOn is new Rx.Op.Observe_On (Operate);
   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxObserveOn.Create;

   -------------
   -- RxPrint --
   -------------

   package RxPrint is new Rx.Op.Print (Operate);
   function Print (Func           : Typed.Actions.Func1Str := null;
                   With_Timestamp : Boolean                := True) return Operator renames RxPrint.Create;

   --------------
   -- RxRepeat --
   --------------

   package RxRepeat is new Rx.Op.Repeat (Operate);

   function Repeat (Times : Rx_Integer) return Operator renames RxRepeat.Repeat;

   function Repeat_Forever return Operator renames RxRepeat.Repeat_Forever;

   function Repeat_Until (Check : Actions.TFilter0'Class) return Operator renames RxRepeat.Repeat_Until;

   function Repeat_Until (Check : Actions.Filter0) return Operator is
     (RxRepeat.Repeat_Until (Actions.Wrap (Check)));

   --------------
   -- RxSample --
   --------------

   package RxSample is new Rx.Op.Sample (Operate, Valueless.Typed);
   use all type RxSample.Policies;
   function Sample (Policy : Policies; Sampler : Valueless.Observable'Class) return Operator is
     (case Policy is
         when Keep_First => RxSample.Create (Keep_First, Sampler),
         when Keep_Last  => RxSample.Create (Keep_Last, Sampler));

   -------------
   -- RxStart --
   -------------

   package RxStart is new Rx.Src.Start (Typed);
   function Start (Func :          Typed.Actions.TFunc0'Class) return Observable renames RxStart.Create;
   function Start (Func : not null Typed.Actions.Func0)        return Observable
     is (Start (Typed.Actions.Wrap (Func)));

   -------------
   -- RxSplit --
   -------------

   procedure Iterate (V        : T_List;
                      For_Each : access procedure (V : T));
   package RxSplit is new Rx.Op.Split (From_List_Transformers, Iterate);
   function Split return From_List_Transformer renames RxSplit.Create;

   -----------------
   -- RxStopwatch --
   -----------------

   package RxStopwatch is new Rx.Op.Stopwatch (Operate);
   function Stopwatch (Proc : not null Actions.Inspector) return Operator renames RxStopwatch.Create;

   -------------
   -- RxStrip --
   -------------

   package RxStrip is new Rx.Op.Map (Into_Valueless);
   function Strip (Dummy : T) return Rx_Nothing is (null record);
   function Strip return Into_Valueless.Operator is (Into_Valueless.Operator (RxStrip.Create (Strip'Access)));

   -----------------
   -- RxSubscribe --
   -----------------

   package RxSubscribe is new Rx.Subscribe (Typed);
   function Subscribe (On_Next      : Typed.Actions.Proc1   := null;
                       On_Complete  : Rx.Actions.Proc0      := null;
                       On_Error     : Rx.Actions.Proc_Error := null) return Sink
   is (RxSubscribe.Create (On_Next, On_Complete , RxSubscribe.Proc_Error (On_Error)));

   function Subscribe (Observer : Typed.Observer) return Sink renames RxSubscribe.Create;

   package RxSubscribeLists is new Rx.Subscribe (Collections.Typed_Lists);
   function Subscribe (On_Next      : Collections.Typed_Lists.Actions.Proc1 := null;
                       On_Complete  : Rx.Actions.Proc0                      := null;
                       On_Error     : Rx.Actions.Proc_Error                 := null)
                       return Collections.Typed_Lists.Sink is
      (RxSubscribeLists.Create (On_Next, On_Complete , RxSubscribeLists.Proc_Error (On_Error)));

   package RxSubsOn is new Rx.Op.Subscribe_On (Operate);
   function Subscribe_On (Scheduler : Schedulers.Scheduler) return Operator renames RxSubsOn.Create;

   ------------
   -- RxTake --
   ------------

   package RxTake is new Rx.Op.Take (Operate);
   function Take  (Count : Rx_Natural) return Operator renames RxTake.Take_Count;
   function Take_Until (Done : not null Typed.Actions.Filter1)        return Operator is
      (RxTake.Take_Until (Typed.Actions.Wrap (Done)));
   function Take_Until (Done :          Typed.Actions.TFilter1'Class) return Operator renames RxTake.Take_Until;
   function Take_While (Pass : not null Typed.Actions.Filter1)        return Operator is
       (RxTake.Take_While (Typed.Actions.Wrap (Pass)));
   function Take_While (Pass :          Typed.Actions.TFilter1'Class) return Operator renames RxTake.Take_While;

   -------------
   -- RxTimer --
   -------------

   package RxTimer is new Rx.Src.Timer (Typed);
   function Timer (V         : T;
                   After     : Duration;
                   Scheduler : Schedulers.Scheduler := Schedulers.Computation)
                   return Observable renames RxTimer.Create;

   --------------
   -- While_Do --
   --------------

   function While_Do (Check : Actions.TFilter0'Class) return Operator renames RxRepeat.While_Do;

   function While_Do (Check : Actions.Filter0) return Operator is
     (RxRepeat.While_Do (Actions.Wrap (Check)));

end Rx.Observables;
