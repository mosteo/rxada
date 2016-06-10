with Rx.From;
with Rx.Subscriptions;
with Rx.Traits.Arrays;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observables is

   ----------
   -- From --
   ----------

   package Default_Arrays is new Rx.Traits.Arrays (Typed, Integer);

   -- Observable from an array of values, useful for literal arrays
   function From (A : Default_Arrays.Typed_Array) return Typed.Producers.Observable'Class;

   ----------
   -- Just --
   ----------

   -- Observable from single value
   function Just (V : Typed.Type_Traits.T) return Typed.Producers.Observable'Class;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Typed.Consumers.Observer'Class;

   ---------
   -- "&" --
   ---------

   --  Subscribe
   function "&" (L : Typed.Producers.Observable'Class;
                 R : Typed.Consumers.Observer'Class)
                 return Subscriptions.Subscription;

private

   package From_Arrays is new Rx.From.From_Array (Default_Arrays);
   function From (A : Default_Arrays.Typed_Array) return Typed.Producers.Observable'Class
     renames From_Arrays.From;

end Rx.Observables;
