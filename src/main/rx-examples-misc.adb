with Rx.Schedulers;
with Rx.Std; use Rx.Std;

procedure Rx.Examples.Misc is
   use Integers.Linkers;
   use Strings.Linkers;
begin
   Integers.Subscribe
     (All_Positives (Count => 999999)
      &
        Integers.Subscribe_On (Schedulers.IO)
      &
        Images.Integers.Print (With_Timestamp => False));

   Strings.Subscribe
     (All_Printable_Strings (Initial => "", Count => 999999)
      &
        Strings.Subscribe_On (Schedulers.IO)
      &
        Images.Strings.Print (With_Timestamp => False));
end Rx.Examples.Misc;
