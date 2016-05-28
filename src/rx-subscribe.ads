with Rx.Producer;

generic
   with package   Binding is new Rx.Producer (<>);
package Rx.Subscribe is

   type Observer is new Binding.Observer with private;
   
   function Create (A : Binding.Action) return not null access Observer;
   
private

   type Observer is new Binding.Observer with record
      A : Binding.Action;
   end record;
   
   overriding
   procedure OnNext (This : in out Observer; V : Binding.T);

end Rx.Subscribe;
