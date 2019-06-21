with Rx.Contracts;
with Rx.Tools.Holders;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Impl.Definite_Observables is

   pragma Preelaborate;

   type Observable is new Contracts.Observable with private;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Contracts.Observer'Class);

   function From (Indef : Contracts.Observable'Class) return Observable;
   function "+"  (Indef : Contracts.Observable'Class) return Observable renames From;

   procedure From (This : in out Observable; Indef : Contracts.Observable'Class);

   function To_Indef (This : Observable) return Contracts.Observable'Class;

   function Is_Valid (This : Observable) return Boolean;

private

   package Obs_Holders is new Rx.Tools.Holders (Contracts.Observable'Class, "definite_observable'class");

   type Observable is new Obs_Holders.Definite and Contracts.Observable with null record;

   function To_Indef (This : Observable) return Contracts.Observable'Class is (This.Get);

   overriding
   function Is_Valid (This : Observable) return Boolean is
      (Obs_Holders.Definite (This).Is_Valid);

end Rx.Impl.Definite_Observables;
