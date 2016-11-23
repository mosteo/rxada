package body Rx.Impl.Semaphores is

   function Tamper is new Shared_Semaphores.Tamper;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Critical_Section) is
   begin
      Tamper (Shared_Semaphores.Proxy (This.Mutex.all)).Seize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Critical_Section) is
   begin
      Tamper (Shared_Semaphores.Proxy (This.Mutex.all)).Release;
   end Finalize;

end Rx.Impl.Semaphores;
