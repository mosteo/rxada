with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.Funnel is

   --  Special internal operator used to implement multiobservers.
   --  Once subscribed, all copies have a shared downstream observer.

   --  The returned operator is thread-safe (via Op.Serialize)

   function Create return Preserver.Operator'Class;
   --  Shared downstream upon On_Subscribe

end Rx.Op.Funnel;
