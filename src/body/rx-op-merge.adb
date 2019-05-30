with Rx.Debug;
with Rx.Errors;
with Rx.Impl.Shared_Observer;
with Rx.Op.Serialize;

package body Rx.Op.Merge is

   package RxSerialize is new Rx.Op.Serialize  (Preserver);

   package Shared_Observers is new Impl.Shared_Observer (Preserver.Typed);

   type Shared_Merger is new Preserver.Typed.Contracts.Observer with record
      Observer : Shared_Observers.Observer;
   end record;

   overriding procedure On_Next (This : in out Shared_Merger; V : Preserver.T);
   overriding procedure On_Complete (This : in out Shared_Merger);
   --  Need to override, because it will be correctly called twice
   overriding procedure On_Error (This : in out Shared_Merger; E : Errors.Occurrence);

   overriding procedure On_Next (This : in out Shared_Merger; V : Preserver.T) is
   begin
      Debug.Trace ("shared_merger on_next");
      This.Observer.On_Next (V);
   end On_Next;

   overriding procedure On_Error (This : in out Shared_Merger; E : Errors.Occurrence) is
   begin
      Debug.Trace ("shared_merger on_error");
      This.Observer.On_Error (E);
   end On_Error;

   type Fake_Merger is new Preserver.Operator with record
      Merge_With : Preserver.Typed.Definite_Observables.Observable;
      Shared     : Shared_Merger;
      Scheduler  : Schedulers.Scheduler;
   end record;
   --  Used as a front, during chaining

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T);
   overriding procedure Subscribe (This     : in out Fake_Merger;
                                   Consumer : in out Preserver.Observer'Class);

   type Real_Merger is new Preserver.Operator with record
      Completed  : Natural := 0;
   end record;
   --  Used as a shared observer, during subscription

   overriding procedure On_Complete  (This : in out Real_Merger);
   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T);

   ------------
   -- Create --
   ------------

   function Create (Merge_With : Preserver.Observable'Class;
                    Observe_On : Schedulers.Scheduler := Schedulers.Immediate)
                    return Preserver.Operator'Class is
   begin
      return M : Fake_Merger do
         M.Merge_With.From (Merge_With);
         M.Scheduler := Observe_On;
      end return;
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete  (This : in out Real_Merger) is
   begin
      Debug.Trace ("real_merger no_complete");
      if This.Is_Subscribed then
         This.Completed := This.Completed + 1;

         if This.Completed = 2 then
            This.Get_Observer.On_Complete;
            This.Unsubscribe;
         end if;
         Debug.Trace ("real_merger on_complete count" & This.Completed'Img);
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Complete;

   overriding procedure On_Complete (This : in out Shared_Merger) is
   begin
      Debug.Trace ("shared_merger on_complete");
      This.Observer.On_Complete_Without_Completion;
   end On_Complete;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T) is
   begin
      Debug.Trace ("fake_merger on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T) is
   begin
      Debug.Trace ("real_merger on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (This     : in out Fake_Merger;
                        Consumer : in out Preserver.Observer'Class)
   is
      Real       : Real_Merger;
      Serialize  : Preserver.Operator'Class := RxSerialize.Create;
      Observe_On : Preserver.Operator'Class := RxObserve.Create (This.Scheduler);
   begin
      Real                .Set_Observer (Consumer);    -- Stores a downstream copy
      This.Shared.Observer.Set_Observer (Real);        -- Create a shared front
      Serialize           .Set_Observer (This.Shared); -- Serialize calls to downstream
      Observe_On          .Set_Observer (Serialize);   -- Switch to given scheduler

      --  Set_Observer is the poor man's way of setting chains during
      --    implementation, before all operators and "&" are available.
      --  Also, at this time we do not know if Consumer is Operator or Sink

      declare
         use all type Schedulers.Scheduler;
         Head : Preserver.Operator'Class :=
                  (if   This.Scheduler = Schedulers.Immediate
                   then Serialize
                   else Observe_On);
      begin
         This.Merge_With.Subscribe (Head);                -- Subscribe to Obs 2
         Preserver.Operator (This).Subscribe (Serialize); -- Subscribe to Obs 1
      end;
   end Subscribe;

end Rx.Op.Merge;
