with Rx.Debug.Observers;
with Rx.Src.Create;
with Rx.Std;

package Rx.Devsupport is

--  For things that must be at library level

   package Create is new Src.Create (Std.Integers.Typed);
   package IntChecker is new Debug.Observers (Std.Integers.Typed, 0, Rx_Integer'Image);

   procedure Blah123 (Observer : in out Std.Integers.Typed.Observer'Class);

end Rx.Devsupport;
