package body Rx.Impl.Semaphores is

   function Tamper is new Shared_Semaphores.Tamper;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Critical_Section) is
   begin
      if not This.Mutex.Is_Valid then
         raise Constraint_Error with "Uninitialized semaphore";
      end if;

--        This.Mutex.Tamper.Seize;
      Tamper (Shared_Semaphores.Proxy (This.Mutex.all)).Seize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Critical_Section) is
   begin
--        This.Mutex.Tamper.Release;
      Tamper (Shared_Semaphores.Proxy (This.Mutex.all)).Release;
   end Finalize;

end Rx.Impl.Semaphores;
