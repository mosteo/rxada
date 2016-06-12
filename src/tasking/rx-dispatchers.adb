package body Rx.Dispatchers is

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
                     Typed.Consumers.Default_Error_Handler (R.Child, E);
               end;
            when On_Error     => R.Child.On_Error (R.E);
            when On_Completed => R.Child.On_Completed;
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
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "On_Completed unimplemented");
         raise Program_Error with "Unimplemented procedure On_Completed";
      end On_Completed;

      --------------
      -- On_Error --
      --------------

      procedure On_Error
        (Sched : in out Dispatcher'Class;
         Observer : Shared.Observer;
         E : Rx.Errors.Occurrence)
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "On_Error unimplemented");
         raise Program_Error with "Unimplemented procedure On_Error";
      end On_Error;

   end Events;

end Rx.Dispatchers;
