with Rx.Base;
with Rx.Consumers;
with Rx.Values;

package Rx.Just is

   type Observable is new Base.Observable with private;

   function Create (V : Values.Value'Class) return Base.Observable'Class;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Consumers.Observer'Class);

private

   type Observable is new Base.Observable with record
      Value : Values.Holder;
   end record;

end Rx.Just;
