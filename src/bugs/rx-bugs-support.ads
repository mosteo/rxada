with Rx.Impl.Tasks;

package Rx.Bugs.Support is

   --  For things that must be at library level

   task type Y is new Impl.Tasks.Transient with end Y;
   type Y_Ptr is access all Y;

end Rx.Bugs.Support;
