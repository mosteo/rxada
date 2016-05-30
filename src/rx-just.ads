with Rx.Base;
with Rx.Holder;
with Rx.Interfaces;

package Rx.Just is

   package I renames Interfaces;

   type Observable is new Base.Observable with private;

   function Create (V : I.Value'Class) return Observable;

   overriding
   procedure Subscribe (Producer : Observable;
                        Consumer : I.Observer'Class);

private

   type Observable is new Base.Observable with record
      Value : Rx.Holder.TH;
   end record;

end Rx.Just;
