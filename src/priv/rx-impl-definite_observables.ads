with Rx.Contracts;
with Rx.Holders;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Impl.Definite_Observables is

   pragma Preelaborate;

   type Observable is new Contracts.Observable with private;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer :        Contracts.Subscriber'Class);

   function From (Indef : Contracts.Observable'Class) return Observable;
   function "+"  (Indef : Contracts.Observable'Class) return Observable renames From;

   procedure From (This : in out Observable; Indef : Contracts.Observable'Class);

   function To_Indef (This : Observable) return Contracts.Observable'Class;

private

   package Obs_Holders is new Rx.Holders (Contracts.Observable'Class);

   type Observable is new Obs_Holders.Definite and Contracts.Observable with null record;

   function To_Indef (This : Observable) return Contracts.Observable'Class is (This.Get);

end Rx.Impl.Definite_Observables;
