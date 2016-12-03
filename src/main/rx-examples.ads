with Rx.Std;
with Rx.Subscriptions;

package Rx.Examples is

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);
   function Inc (I : Integer) return Integer is (I+1);

   Nosub : Subscriptions.No_Subscription;
   Sub   : Subscriptions.Subscription;

   --  Finally, to increase ambiguity:
   --  package Chars is new Rx.Definites (Character); -- THIS LINE BREAKS SOMETHING

   type Intarr is array (Integer range <>) of Integer;


end Rx.Examples;
