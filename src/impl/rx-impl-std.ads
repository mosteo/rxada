with Rx.Definites;
with Rx.Indefinites;

package Rx.Impl.Std is

   package Any      is new Rx.Indefinites (Rx_Any'Class);
   package Floats   is new Rx.Definites (Rx_Float);
   package Integers is new Rx.Definites (Rx_Integer);
   package Strings  is new Rx.Indefinites (Rx_String);

end Rx.Impl.Std;
