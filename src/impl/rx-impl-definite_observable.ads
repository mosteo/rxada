with Rx.Contracts;
with Rx.Holders;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Impl.Definite_Observable is

   pragma Preelaborate;

   type Observable is new Contracts.Observable with private;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Contracts.Subscriber'Class);

   function From (Indef : Contracts.Observable'Class) return Observable;
   function "+"  (Indef : Contracts.Observable'Class) return Observable renames From;

   procedure From (This : in out Observable; Indef : Contracts.Observable'Class);

private

   package Obs_Holders is new Rx.Holders (Contracts.Observable'Class);

   type Observable is new Obs_Holders.Definite and Contracts.Observable with null record;

end Rx.Impl.Definite_Observable;
