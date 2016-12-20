--  For ad-hoc testing

with Rx.Debug;
with Rx.Devsupport;
with Rx.Std;

procedure Rx.Devel is

   use Rx.Devsupport.IntChecker;
   use Rx.Std;
   use Rx.Std.Integers;

   procedure Blah123 (Observer : in out Std.Integers.Typed.Subscriber) is
   begin
      Observer.On_Next (1);
      delay 0.2;
      Observer.On_Next (2);
      delay 0.2;
      Observer.On_Next (3);
      Observer.On_Next (4);
      Observer.On_Next (5);
      Observer.On_Completed;
   end Blah123;

begin
   for I in 1 .. 99 loop
   For_Each (Integers.RxCreate.Parameterless (Blah123'Access)
             & Debounce (0.1)
             & Print (Debug.Image'Access),
             Subscribe_Checker (Do_First => True,  Ok_First => 1,
                                Do_Last  => True,  Ok_Last  => 5,
                                Do_Count => True,  Ok_Count => 3));
   end loop;
end Rx.Devel;
