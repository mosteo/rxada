package body Rx.Impl.Semaphores is

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Critical_Section) is
   begin
      This.Mutex.Sem.Seize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Critical_Section) is
   begin
      This.Mutex.Sem.Release;
   end Finalize;

end Rx.Impl.Semaphores;
