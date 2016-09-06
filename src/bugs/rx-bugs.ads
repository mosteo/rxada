with Rx.Std;

package Rx.Bugs is

private

   package IntCount is new Std.Integers.Counters (Integer'Succ);

end Rx.Bugs;
