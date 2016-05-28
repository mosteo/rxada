with Rx.Holder;
with Rx.Producer;

generic
   with package Binding is new Rx.Producer (<>);
package Rx.Just is   
   
   subtype T is Binding.T;
   
   function Create (Val : T) return not null access Binding.Observable'Class;

private

   package Holder is new Rx.Holder (T);

   type Observable is new Binding.Observable with record
      V : Holder.TH;
   end record;

   overriding 
   procedure Subscribe (O : in out Observable;
                        S : access Binding.Observer'Class);

end Rx.Just;
