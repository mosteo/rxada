--  For ad-hoc testing

--  with Rx.Debug;
with Rx.Devsupport;
with Rx.Std;

procedure Rx.Devel is

   use Rx.Devsupport.IntChecker;
   use Rx.Std;
   use Rx.Std.Integers;

begin
   For_Each (Numeric.Integers.Range_Count (1, Count => 9_999_999),
             Subscribe_Checker (Do_First => True, Ok_First => 1, Do_Watch => False));

--     delay 5.0;
end Rx.Devel;
