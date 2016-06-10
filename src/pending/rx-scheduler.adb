package body Rx.Scheduler is

   ------------
   -- Events --
   ------------

   package body Events is

      -------------
      -- On_Next --
      -------------

      procedure On_Next
        (Sched : in out Object'Class;
         Observer : Shared.Observer;
         V : Typed.Type_Traits.T)
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "On_Next unimplemented");
         raise Program_Error with "Unimplemented procedure On_Next";
      end On_Next;

      ------------------
      -- On_Completed --
      ------------------

      procedure On_Completed
        (Sched : in out Object'Class;
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
        (Sched : in out Object'Class;
         Observer : Shared.Observer;
         E : Rx.Errors.Occurrence)
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "On_Error unimplemented");
         raise Program_Error with "Unimplemented procedure On_Error";
      end On_Error;

   end Events;

end Rx.Scheduler;
