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
         Child : Shared.Subscriber;
      end record;

      overriding procedure Run (R : Runner) is
         use all type Base.Kinds;
         RW : Runner := R; -- Local writable copy
      begin
         case R.Kind is
            when On_Next      =>
               begin
                  RW.Child.On_Next (Base.Value (R.Event));
               exception
                  when E : others =>
                     Typed.Default_Error_Handler (RW.Child, E);
               end;
            when On_Error     =>
               declare
                  E : Errors.Occurrence := Base.Error (R.Event);
               begin
                  RW.Child.On_Error (E);
                  if not E.Is_Handled then
                     E.Reraise; -- Because we are in a new thread, the Error won't go any further
                  end if;
               end;
            when On_Completed =>
               RW.Child.On_Completed;
            when Unsubscribe =>
               Rw.Child.Unsubscribe;
         end case;
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

      -----------------
      -- Unsubscribe --
      -----------------

      procedure Unsubscribe  (Sched : in out Dispatcher'Class; Observer : Shared.Subscriber) is
      begin
         Sched.Schedule (Runner'(Base.Unsubscribe, Base.Unsubscribe, Observer));
      end Unsubscribe;

   end Events;

   package body Subscribe is

      type Runner is new Runnable with record
         Op : Operate.Holders.Definite;
      end record;

      overriding procedure Run (R : Runner) is
         Parent : Operate.Observable := R.Op.CRef.Get_Parent;
         Child  : Operate.Subscriber := R.Op.CRef;
      begin
         Parent.Subscribe (Child);
      end Run;

      procedure On_Subscribe (Sched : in out Dispatcher'Class; Operator : Operate.Preserver'Class) is
      begin
         Sched.Schedule (Runner'(Runnable with Operate.Holders.Hold (Operator)));
      end On_Subscribe;

   end Subscribe;

end Rx.Dispatchers;
