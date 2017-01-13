with Ada.Finalization;
with Ada.Unchecked_Deallocation;

procedure B003_Taskleak is

   package Inner is

      type Some_Task is task interface;
      type Some_Ptr is access all Some_Task'Class;

      type Wrapper (Ptr : Some_Ptr) is limited private;

      task type T is new Inner.Some_Task with end T;

   private

      type Wrapper (Ptr : Some_Ptr) is new Ada.Finalization.Limited_Controlled with null record;

      overriding procedure Initialize (W : in out Wrapper);

   end Inner;

   package body Inner is

      overriding procedure Initialize (W : in out Wrapper) is
         procedure Free is new Ada.Unchecked_Deallocation (Some_Task'Class, Some_Ptr);
         Ptr : Some_Ptr := W.Ptr;
      begin
         Free (Ptr);
      end Initialize;

      task body T is
         W : Inner.Wrapper (T'Unchecked_Access);
      begin
         delay 1.0;
      end T;

   end Inner;

   procedure Leak is
      Ptr : Inner.Some_Ptr := new Inner.T;
   begin
      null;
   end Leak;

begin
   for I in 1 .. 99 loop
      Leak;
   end loop;
end B003_Taskleak;
