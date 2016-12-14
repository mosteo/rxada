with Ada.Unchecked_Deallocation;

procedure B002_Taskiface is
   type Some_Task is task interface;

   type Some_Ptr is access Some_Task'Class;

   task type T is new Some_Task with end T;

   type T_Ptr is access T;

   task body T is
   begin
      null;
   end T;

   procedure Free_S is new Ada.Unchecked_Deallocation (Some_Task'Class, Some_Ptr);
   procedure Free_T is new Ada.Unchecked_Deallocation (T, T_Ptr);

   T_Arr : array (1 .. 99) of T_Ptr    := (others => new T);
   S_Arr : array (1 .. 99) of Some_Ptr := (others => new T);

begin
   delay 2.0; -- Wait for tasks termination

   for T of T_Arr loop
      Free_T (T);
   end loop;

   for S of S_Arr loop
      Free_S (S);
   end loop;
end B002_Taskiface;
