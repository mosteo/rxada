--  For ad-hoc testing

with Rx.Debug;
with Rx.Devsupport;
with Rx.Op.Subscribe_On;
with Rx.Std;

procedure Rx.Devel is

   use Rx.Devsupport.IntChecker;
   use Rx.Std;
   use Rx.Std.Integers;

   package S is new Rx.Op.Subscribe_On (Std.Integers.Operate);

begin
   null;
end Rx.Devel;
