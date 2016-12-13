with Ada.Calendar;
with Ada.Unchecked_Deallocation;

with Rx.Debug;
with Rx.Dispatchers;
with Rx.Dispatchers.Single;

package body Rx.Impl.Tasks is

   Grace_Period : constant Duration := 1.0;

   DEATH : Rx.Dispatchers.Single.Dispatcher;

   procedure Free is new Ada.Unchecked_Deallocation (Transient'Class, Transient_Ptr);

   --------------
   -- Reap_Now --
   --------------

   procedure Reap_Now (This : in out Transient_Ptr) is
   begin
      if This /= null then
         while not This.all'Terminated loop
            delay 0.01;
         end loop;
         Free (This);
      end if;
   end Reap_Now;

   type Reap_Attempt is new Dispatchers.Runnable with record
      Victim : Transient_Ptr;
   end record;

   ---------
   -- Run --
   ---------

   overriding procedure Run (This : Reap_Attempt) is
      use Ada.Calendar;
      Victim : Transient_Ptr := This.Victim;
   begin
      Debug.Log ("HUSH", Debug.Warn);
      if This.Victim.all'Terminated then
         Free (Victim);
         Debug.Log ("RIP", Debug.Warn);
      else
         DEATH.Schedule (This, Clock + Grace_Period);
         Debug.Log ("YOU'LL BE MINE", Debug.Warn);
      end if;
   end Run;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Reaper) is
      use Ada.Calendar;
   begin
      DEATH.Schedule (Reap_Attempt'(Victim => This.Victim), Clock + Grace_Period);
   end Finalize;

end Rx.Impl.Tasks;
