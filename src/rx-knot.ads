with Rx.Producer;

generic
   type T (<>) is private;
package Rx.Knot is

   package Binding is new Rx.Producer (T);

   type Observable (<>) is new Binding.Observable with private;

   function Just (Val : T) return Observable'Class;
   
   overriding
   procedure Subscribe (O : in out Observable; 
                        S : access Binding.Observer'Class);                        
                        
   procedure Subscribe (O : in out Observable;
                        A : Binding.Action := null);
   
private

   type Observable (Real : not null access Binding.Observable'Class) is 
      new Binding.Observable with null record;

end Rx.Knot;
