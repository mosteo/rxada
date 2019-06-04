with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.Hold is

   function Create (Fixed  : Duration;
                    Random : Duration := 0.0)
                    return Preserver.Operator'Class;
   --  Hold items for a certain time before releasing them
   --  A fixed amount is always applied, and a random one on top
   --  This is a blocking delay! Use appropriate operators to schedule

end Rx.Op.Hold;
