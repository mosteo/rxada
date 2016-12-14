with Rx.Impl.Task_Deallocation;
with Rx.Impl.Tasks;

package Rx.Bugs.Support is

   --  For things that must be at library level

   task type X is new Impl.Tasks.Transient with end X;
   type X_Ptr is access all X;

   --  X uses Task_Deallocation

   task type Y is new Impl.Tasks.Transient with end Y;
   type Y_Ptr is access all Y;

   --  Y uses Tasks

   package Reap is new Impl.Task_Deallocation (X, X_Ptr);

end Rx.Bugs.Support;
