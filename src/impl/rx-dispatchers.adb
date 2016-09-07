package body Rx.Dispatchers is

   Shutting_Down : Boolean := False;
   pragma Atomic (Shutting_Down);

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

      type Kinds is (On_Next, On_Completed, On_Error);

      type Runner (Kind : Kinds) is new Runnable with record
         Child : Shared.Observer;
         case Kind is
            when On_Next      => V : Typed.D;
            when On_Error     => E : Errors.Occurrence;
            when On_Completed => null;
         end case;
      end record;

      overriding procedure Run (R : in out Runner) is
         use Typed.Type_Traits;
      begin
         case R.Kind is
            when On_Next      =>
               begin
                  R.Child.On_Next (+R.V);
               exception
                  when E : others =>
                     Typed.Default_Error_Handler (R.Child, E);
               end;
            when On_Error     =>
               R.Child.On_Error (R.E);
               if not R.E.Is_Handled then
                  R.E.Reraise; -- Because we are in a new thread, the Error won't go any further
               end if;
            when On_Completed =>
               R.Child.On_Completed;
         end case;
      end Run;

      -------------
      -- On_Next --
      -------------

      procedure On_Next
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer;
         V : Typed.Type_Traits.T)
      is
         use Typed.Type_Traits;
         R : Runner := (On_Next, Observer, +V); -- Create a copy so it's in/out
      begin
         Sched.Schedule (R);
      end On_Next;

      ------------------
      -- On_Completed --
      ------------------

      procedure On_Completed
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer)
      is
         R : Runner := (On_Completed, Observer);
      begin
         Sched.Schedule (R);
      end On_Completed;

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer;
         E : Rx.Errors.Occurrence)
      is
         R : Runner := (On_Error, Observer, E);
      begin
         Sched.Schedule (R);
      end On_Error;

   end Events;

   package body Subscribe is

      type Runner is new Runnable with record
         Op : Operate.Holders.Definite;
      end record;

      overriding procedure Run (R : in out Runner) is
         Parent : Operate.Observable := R.Op.CRef.Get_Parent;
      begin
         Parent.Subscribe (R.Op.Ref); -- Suspicious... should make a copy of R.Op?
      end Run;

      procedure On_Subscribe (Sched : in out Dispatcher'Class; Operator : Operate.Operator'Class) is
         R : Runner := (Runnable with Operate.Holders.Hold (Operator));
      begin
         Sched.Schedule (R);
      end On_Subscribe;

   end Subscribe;

end Rx.Dispatchers;
