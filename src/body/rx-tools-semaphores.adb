with Rx.Debug;

package body Rx.Tools.Semaphores is

   ---------------
   -- Reentrant --
   ---------------

   protected body Reentrant is

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Count := Count - 1;
         Debug.Trace ("Releasing [count]" & Count'Img);
      end Release;

      -----------
      -- Seize --
      -----------

      entry Seize when True is
         use Ada.Task_Identification;
      begin
         if Reentrant.Seize'Caller = Owner then
            Count := Count + 1;
            Debug.Trace ("Seizing [count]" & Count'Img & " @ " & Image (Owner));
         else
            Debug.Trace ("Waiting [count]" & Count'Img & " @ " & Image (Reentrant.Seize'Caller));
            requeue Wait with abort;
         end if;
      end Seize;

      ----------
      -- Wait --
      ----------

      entry Wait when Count = 0 is
         use Ada.Task_Identification;
      begin
         Debug.Trace ("Seizing [wait] @ " & Image (Wait'Caller));
         Count := 1;
         Owner := Wait'Caller;
      end Wait;

   end Reentrant;

   function Tamper is new Shared_Semaphores.Tamper;

   subtype Proxy is Shared_Semaphores.Proxy;

   -----------
   -- Seize --
   -----------

   not overriding procedure Seize (This : in out Shared) is
   begin
      if not This.Fake then
         Debug.Trace ("outer seize " & This.Image);
         Tamper (Proxy (This)).Seize;
      end if;
   end Seize;

   -------------
   -- Release --
   -------------

   not overriding procedure Release (This : in out Shared) is
   begin
      if not This.Fake then
         Debug.Trace ("outer release " & This.Image);
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

end Rx.Tools.Semaphores;
