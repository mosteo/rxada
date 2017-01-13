with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Src.Create is

   package body With_State is

      type Source is new Typed.Contracts.Observable with record
         Initial : State;
      end record;

      ---------------
      -- Subscribe --
      ---------------

      overriding procedure Subscribe
        (Producer : in out Source;
         Consumer : in out Typed.Subscriber)
      is
         Actual : Typed.Subscriber := Consumer;
      begin
         begin
            On_Subscribe (Producer.Initial, Actual);
         exception
            when Subscriptions.No_Longer_Subscribed =>
               Debug.Log ("At Create.Subscribe: caught No_Longer_Subscribed", Debug.Note);
            when E : others =>
               if Autocompletes then -- Because the error was within On_Next somewhere
                  Typed.Defaults.Default_Error_Handler (Actual, E);
               else
                  raise; -- Otherwise either client properly treated or wrong client implementation
               end if;
         end;

         if Autocompletes then
            Actual.On_Completed;
         end if;
      exception
         when Subscriptions.No_Longer_Subscribed =>
            Debug.Log ("At Create.Subscribe: caught No_Longer_Subscribed", Debug.Note);
      end Subscribe;

      ------------
      -- Create --
      ------------

      function Create (Initial : State) return Typed.Observable is
      begin
         return Source'(Initial => Initial);
      end Create;

   end With_State;

   -------------------
   -- Parameterless --
   -------------------

   type Parameterless_Proc is access procedure (Observer : in out Typed.Subscriber);
   procedure On_Subscribe (Initial : Parameterless_Proc; Observer : in out Typed.Subscriber) is
   begin
      Initial (Observer);
   end On_Subscribe;

   package Create_Parameterless is new With_State (Parameterless_Proc, On_Subscribe, Autocompletes => False);

   function Parameterless (On_Subscribe : not null access procedure (Observer : in out Typed.Subscriber))
                           return Typed.Observable is
   begin
      return Create_Parameterless.Create (On_Subscribe);
   end Parameterless;

end Rx.Src.Create;
