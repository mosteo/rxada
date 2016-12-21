--  For ad-hoc testing

with Rx.Debug;
with Rx.Devsupport;
with Rx.Std;

procedure Rx.Devel is

   use Rx.Devsupport.IntChecker;
   use Rx.Std;
   use Rx.Std.Integers;

begin
   For_Each (Interval,
             Subscribe_Checker (Do_Count => True, Ok_Count => 1));

   delay 5.0;
end Rx.Devel;
