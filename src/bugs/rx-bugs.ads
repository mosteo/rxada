with Rx.Integers;

package Rx.Bugs is

private

   use Rx.Integers.Observables;
   package IntCount is new Rx.Integers.Observables.Counters (Integer'Succ);

end Rx.Bugs;
