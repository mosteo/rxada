with Rx.Values;

package Rx.Subscribers is

   pragma Pure;

   type Observer is interface;
   procedure OnNext      (This : Observer; V : Values.Value'Class) is abstract;
   procedure OnCompleted (This : Observer) is null;

end Rx.Subscribers;
