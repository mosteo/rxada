with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Serialize is

   function Create return Operate.Operator'Class;

   --  Serializes calls to On_*
   --  Does not serialize calls to Subscribe/Unsubscribe
   --    which, unless something very strange is happening,
   --    should happen from a single thread

end Rx.Op.Serialize;
