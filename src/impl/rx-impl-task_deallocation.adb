with Ada.Unchecked_Deallocation;

procedure Rx.Impl.Task_Deallocation (T : Task_Ptr) is

   procedure Free is new Ada.Unchecked_Deallocation (Task_Type, Task_Ptr);

   RW : Task_Ptr := T;

begin
   Free (RW);
end Rx.Impl.Task_Deallocation;
