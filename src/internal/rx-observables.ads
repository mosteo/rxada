with Rx.From;
with Rx.Subscriptions;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observables is


   ----------
   -- Just --
   ----------

   -- Observable from single value
   function Just (V : Typed.Type_Traits.T) return Typed.Producers.Observable'Class;

   ----------
   -- From --
   ----------

   package Default_From is new Rx.From (Typed);
   package From_Array is new Default_From.From_Array (Integer);

   -- Observable from an array of values, useful for literal arrays
   function From (A : From_Array.Array_Type) return Typed.Producers.Observable'Class renames From_Array.From;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (On_Next : Typed.Actions.Proc1 := null) return Typed.Consumers.Observer'Class;

   function "&" (L : Typed.Producers.Observable'Class;
                 R : Typed.Consumers.Observer'Class)
                 return Subscriptions.Subscription;

end Rx.Observables;
