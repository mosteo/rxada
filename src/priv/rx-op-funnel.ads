with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.Funnel is

   --  Special internal operator used to implement multiobservers.
   --  Once chained, all copies have a shared downstream observer.

   --  The returned operator is thread-safe (via Op.Serialize)

   function Create return Preserver.Operator'Class;

end Rx.Op.Funnel;
