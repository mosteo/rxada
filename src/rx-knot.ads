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
                        
   procedure Subscribe (O : Observable;
                        A : Binding.Action := null);
   
   generic
      with package Downstream is new Rx.Producer (T);
   package Operators is 
      
      type Operator is access function (Val : T) return Downstream.T;
      
      function Map (Op : Operator) return Downstream.Observable'Class;
      
   end Operators;
   
private

   type Observable (Real : not null access Binding.Observable'Class) is 
      new Binding.Observable with null record;

end Rx.Knot;
