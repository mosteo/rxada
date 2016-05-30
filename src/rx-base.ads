with Rx.Interfaces;

package Rx.Base is

   package I renames Rx.Interfaces;

   type Observable is abstract new I.Observable with null record;

end Rx.Base;
