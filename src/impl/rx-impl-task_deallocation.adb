with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Deallocation;

with Rx.Debug;
with Rx.Dispatchers.Single;

package body Rx.Impl.Task_Deallocation is

   Grace_Period : constant Duration := 1.0;

   DEATH : Rx.Dispatchers.Single.Dispatcher;

   procedure Free is new Ada.Unchecked_Deallocation (Task_Type, Ptr);

   ------------------
   -- Reap_Attempt --
   ------------------

   type Reap_Attempt is new Dispatchers.Runnable with record
      Id     : Ada.Task_Identification.Task_Id;
      Victim : Ptr;
   end record;

   ---------
   -- Run --
   ---------

   overriding procedure Run (This : Reap_Attempt) is
      Victim : Ptr := This.Victim;
   begin
      Debug.Log ("HUSH", Debug.Warn);
      if Ada.Task_Identification.Is_Terminated (This.Id) then
         Free (Victim);
         Debug.Log ("RIP", Debug.Warn);
      else
         DEATH.Schedule (This, Clock + Grace_Period);
         Debug.Log ("YOU'LL BE MINE", Debug.Warn);
      end if;
   end Run;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Reaper) is
   begin
      Debug.Log ("Birthing", Debug.Warn);
      This.Id := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Reaper) is
   begin
      Debug.Log ("Dying", Debug.Warn);
      DEATH.Schedule (Reap_Attempt'(Id     => This.Id,
                                    Victim => This.Victim),
                      Clock + Grace_Period);
   end Finalize;

end Rx.Impl.Task_Deallocation;
