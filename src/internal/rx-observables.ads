with Rx.From;
with Rx.Operate;
with Rx.Schedulers;
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
   function Count (First : T) return Operator;

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
   function Just (V : Typed.Type_Traits.T) return Observable;

   ----------------
   -- Observe_On --
   ----------------

   function Observe_On (Scheduler : Schedulers.Scheduler) return Operator;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Observer;

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
                 return Subscriptions.Subscription;

private

   package From_Arrays is new Rx.From.From_Array (Default_Arrays);
   function From (A : Default_Arrays.Typed_Array) return Observable
     renames From_Arrays.From;

end Rx.Observables;
