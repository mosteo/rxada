with Rx.Debug;
with Rx.Impl.Events;
with Rx.Subscriptions;

package body Rx.Dispatchers is

   Shutting_Down : Boolean := False
     with Atomic;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Shutting_Down := True;
   end Shutdown;

   -----------------
   -- Terminating --
   -----------------

   function Terminating return Boolean is (Shutting_Down);

   ------------
   -- Events --
   ------------

   package body Events is

      use Typed.Conversions;

      package Base is new Impl.Events (Typed);

      type Runner (Kind : Base.Kinds) is new Runnable with record
         Event : Base.Event (Kind);
         Downstream : Shared.Subscriber;
      end record;

      overriding procedure Run (R : Runner) is
         use all type Base.Kinds;
         RW : Runner := R; -- Local writable copy
      begin
         case R.Kind is
            when On_Next =>
               begin
                  RW.Downstream.On_Next (Base.Value (R.Event));
               exception
                  when E : others =>
                     Typed.Default_Error_Handler (RW.Downstream, E);
               end;
            when On_Error =>
               RW.Downstream.On_Error (Base.Error (R.Event));
            when On_Completed =>
               RW.Downstream.On_Completed;
         end case;
      exception
         when Subscriptions.No_Longer_Subscribed =>
            Debug.Log ("Dispatchers.Runner caught Not_Longer_Subscribed", Debug.Note);
      end Run;

      -------------
      -- On_Next --
      -------------

      procedure On_Next
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Subscriber;
         V : Typed.Type_Traits.T)
      is
      begin
         Sched.Schedule (Runner'(Base.On_Next, Base.On_Next (V), Observer));
      end On_Next;

      ------------------
      -- On_Completed --
      ------------------

      procedure On_Completed
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Subscriber)
      is
      begin
         Sched.Schedule (Runner'(Base.On_Completed, Base.On_Completed, Observer));
      end On_Completed;

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Subscriber;
         E : Rx.Errors.Occurrence)
      is
      begin
         Sched.Schedule (Runner'(Base.On_Error, Base.On_Error (E), Observer));
      end On_Error;

   end Events;

   package body Subscribe is

      type Runner is new Runnable with record
         Parent : Operate.Typed.Definite_Observables.Observable;
         Child  : Operate.Typed.Holders.Subscribers.Definite;
      end record;

      overriding procedure Run (R : Runner) is
         Parent : Operate.Typed.Observable'Class := R.Parent.To_Indef;
      begin
         Parent.Subscribe (R.Child.CRef);
      end Run;

      procedure On_Subscribe (Sched  : in out Dispatcher'Class;
                              Parent :        Operate.Observable'Class;
                              Child  :        Operate.Typed.Subscriber'Class) is
      begin
         Sched.Schedule (Runner'(Runnable with
                           Parent => Operate.Typed.Definite_Observables.From (Parent),
                           Child  => Operate.Typed.Holders.Subscribers.Hold (Child)));
      end On_Subscribe;

   end Subscribe;

end Rx.Dispatchers;
