with Rx.Contracts;

package Rx.Typeless with Preelaborate is

--  Pre-instance that all typed observables know and hence can use interchangeably without requiring
--  extra-instantiations

   type Rx_Empty is null record;

   package Contracts is new Rx.Contracts (Rx_Empty);

   subtype Observable is Contracts.Observable;

   type Stripper

   function Strip return Observable'Class;

   generic
      with package Typed_Contracts is new Rx.Contracts (<>);
   package Strip is

      function "&" (Producer : Typed_Contracts.Observable'Class;
                    Consumer :

   end Strip;

end Rx.Typeless;
