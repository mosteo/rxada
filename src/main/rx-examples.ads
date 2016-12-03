with Rx.Subscriptions;

package Rx.Examples is

   function Length (S : Rx_String) return Rx_Integer is (S'Length);
   function Image  (I : Rx_Integer) return Rx_String is (I'Img);
   function Inc (I : Rx_Integer) return Rx_Integer is (I+1);

   Nosub : Subscriptions.No_Subscription;
   Sub   : Subscriptions.Subscription;

   --  Finally, to increase ambiguity:
   --  package Chars is new Rx.Definites (Character); -- THIS LINE BREAKS SOMETHING

   type Intarr is array (Rx_Integer range <>) of Rx_Integer;


end Rx.Examples;
