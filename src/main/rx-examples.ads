with Rx.Std;
with Rx.Subscriptions;

package Rx.Examples is

   package Integers renames Std.Integers;
   package Strings  renames Std.Strings;

   package IntToStr renames Std.IntToStr;
   package StrToInt renames Std.StrToInt;

   package IntCount renames Std.IntCount;
   package StrCount renames Std.StrCount;

   function Length (S : String) return Integer is (S'Length);
   function Image  (I : Integer) return String is (I'Img);
   function Inc (I : Integer) return Integer is (I+1);

   Chain : Subscriptions.No_Subscription;

   --  Finally, to increase ambiguity:
--   package Chars is new Rx.Definites (Character); -- THIS LINE BREAKS SOMETHING

   type Intarr is array (Integer range <>) of Integer;
--     package Intarrs is new Rx.Indefinites (Intarr);

end Rx.Examples;
