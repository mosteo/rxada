with Rx.Contracts;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Factories with Preelaborate is

   type Observable_Factory is interface;

   function Subscribe (F : Observable_Factory) return Contracts.Observable'Class is abstract;

   type Observable_Factory_Func is access function return Contracts.Observable'Class;

end Rx.Factories;
