with Rx.Actions;
with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.Stopwatch is

   function Create (Callback : not null Actions.Inspector)
                    return Preserver.Operator'Class;
   --  Calls its inspector on every event, with the cumulative and differential
   --  elapsed time.

end Rx.Op.Stopwatch;
