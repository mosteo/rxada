with Rx.Base;
with Rx.Holder;
with Rx.Subscribers;
with Rx.Values;

package Rx.Just is

   type Observable is new Base.Observable with private;

   function Create (V : Values.Value'Class) return Base.Observable'Class;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Subscribers.Observer'Class);

private

   type Observable is new Base.Observable with record
      Value : Rx.Holder.TH;
   end record;

end Rx.Just;
