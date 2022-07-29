with Rx.Debug;
with Rx.Impl.Events;

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
         Downstream : Shared.Observer;
      end record;

      ---------
      -- Run --
      ---------

      overriding procedure Run (R : Runner) is
         use all type Base.Kinds;
         RW : Runner := R; -- Local writable copy
      begin
         case R.Kind is
            when On_Next =>
               begin
                  RW.Downstream.On_Next (Base.Value (R.Event));
               exception
                  when No_Longer_Subscribed =>
                     raise;
                  when E : others =>
                     Typed.Defaults.Default_Error_Handler (RW.Downstream, E);
               end;
            when On_Error =>
               RW.Downstream.On_Error (Base.Error (R.Event));
            when On_Complete  =>
               RW.Downstream.On_Complete ;
         end case;
      exception
         when No_Longer_Subscribed =>
            RW.Downstream.Mark_Completed;
            Debug.Log ("Dispatchers.Runner caught Not_Longer_Subscribed", Debug.Note);
      end Run;

      -------------
      -- On_Next --
      -------------

      procedure On_Next
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer;
         V : Typed.Type_Traits.T)
      is
      begin
         if not Observer.Is_Completed then 
            Sched.Schedule (Runner'(Base.On_Next, Base.On_Next (V), Observer));
         else
            raise No_Longer_Subscribed;
         end if;
      end On_Next;

      ------------------
      -- On_Complete  --
      ------------------

      procedure On_Complete 
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer)
      is
      begin
         if Observer.Is_Completed then 
            raise No_Longer_Subscribed;
         else 
            Sched.Schedule (Runner'(Base.On_Complete , Base.On_Complete , Observer));
         end if;
      end On_Complete ;

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer;
         E : Rx.Errors.Occurrence)
      is
      begin
         if Observer.Is_Completed then 
            raise No_Longer_Subscribed;
         else 
            Sched.Schedule (Runner'(Base.On_Error, Base.On_Error (E), Observer));
         end if;
      end On_Error;

   end Events;

   package body Subscribe is

      type Runner is new Runnable with record
         Parent : Operate.Typed.Definite_Observables.Observable;
         Child  : Operate.Typed.Holders.Observers.Definite;
      end record;

      ---------
      -- Run --
      ---------

      overriding procedure Run (R : Runner) is
         Parent : Operate.Typed.Observable'Class := R.Parent.To_Indef;
         Child  : Operate.Typed.Observer'Class := R.Child.Get;
      begin
         Parent.Subscribe (Child);
      end Run;

      ------------------
      -- On_Subscribe --
      ------------------

      procedure On_Subscribe (Sched  : in out Dispatcher'Class;
                              Parent :        Operate.Observable'Class;
                              Child  :        Operate.Typed.Observer'Class) is
      begin
         Sched.Schedule (Runner'(Runnable with
                           Parent => Operate.Typed.Definite_Observables.From (Parent),
                           Child  => Operate.Typed.Holders.Observers.Hold (Child)));
      end On_Subscribe;

   end Subscribe;

end Rx.Dispatchers;
