package body Rx.Impl.Semaphores is

   function Tamper is new Shared_Semaphores.Tamper;

   subtype Proxy is Shared_Semaphores.Proxy;

   -----------
   -- Seize --
   -----------

   not overriding procedure Seize (This : in out Shared_Binary) is
   begin
      if not This.Fake then
         Tamper (Proxy (This)).Seize;
      end if;
   end Seize;

   -------------
   -- Release --
   -------------

   not overriding procedure Release (This : in out Shared_Binary) is
   begin
      if not This.Fake then
         Tamper (Proxy (This)).Release;
      end if;
   end Release;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Critical_Section) is
   begin
      if This.Mutex.Fake then
         null;
      elsif not This.Mutex.Is_Valid then
         raise Constraint_Error with "Uninitialized semaphore";
      else
         This.Sem := This.Mutex.all;
         --  We make a local copy so that the semaphore exists until release, even if it is destroyen in the
         --  critical section
         This.Sem.Seize;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Critical_Section) is
   begin
      if This.Sem.Is_Valid then
         This.Sem.Release;
      end if;
   end Finalize;

end Rx.Impl.Semaphores;
