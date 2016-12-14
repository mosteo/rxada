-- with B002_Pkg;

procedure B002_Taskiface is

   package Inner is

      type Some_Task is task interface;

      type Some_Ptr is access all Some_Task'Class;

      type Wrapper (Ptr : Some_Ptr) is limited private;

   private

      type Wrapper (Ptr : Some_Ptr) is limited null record;

   end Inner;

   task type T is new Inner.Some_Task with end T;

   task body T is
      S : Inner.Some_Ptr := T'Unchecked_Access;
      W : Inner.Wrapper (T'Unchecked_Access);
   begin
      if T'Terminated then null; end if;
  --    if W.Ptr.all'Terminated then null; end if; -- Likewise fails
      if S.all'Terminated then null; end if;
   end T;

begin
   null;
end B002_Taskiface;
