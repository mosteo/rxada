package Rx.Impl.Tasks with Preelaborate is

   --  Root task interface for short-lived tasks
   --  Tasks of this kind can be reaped more easily

   type Transient is task interface;

   type Transient_Ptr is access all Transient'Class;

   procedure Reap_Now (This : in out Transient_Ptr);
   --  Will block until the task has Terminated

end Rx.Impl.Tasks;
