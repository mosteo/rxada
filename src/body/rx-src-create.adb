with Rx.Debug;

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
         Consumer : in out Typed.Observer)
      is
         Actual       : Typed.Observer := Consumer;
         Unsubscribed : Boolean := False;
      begin
         begin
            On_Subscribe (Producer.Initial, Actual);
         exception
            when No_Longer_Subscribed =>
               Unsubscribed := True;
               Debug.Log ("At Create.Subscribe: caught No_Longer_Subscribed", Debug.Note);
            when E : others =>
               if Autocompletes then -- Because the error was within On_Next somewhere
                  Typed.Defaults.Default_Error_Handler (Actual, E);
               else
                  raise; -- Otherwise either client properly treated or wrong client implementation
               end if;
         end;

         if Autocompletes and not Unsubscribed then
            Actual.On_Complete ;
         end if;
      exception
         when No_Longer_Subscribed =>
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

   type Parameterless_Proc is access procedure (Observer : in out Typed.Observer);
   procedure On_Subscribe (Initial : Parameterless_Proc; Observer : in out Typed.Observer) is
   begin
      Initial (Observer);
   end On_Subscribe;

   --------------------------
   -- Create_Parameterless --
   --------------------------

   package Create_Parameterless is new With_State (Parameterless_Proc, On_Subscribe, Autocompletes => False);

   -------------------
   -- Parameterless --
   -------------------

   function Parameterless (On_Subscribe : not null access procedure (Observer : in out Typed.Observer))
                           return Typed.Observable is
   begin
      return Create_Parameterless.Create (On_Subscribe);
   end Parameterless;

   ----------------
   -- Enumerator --
   ----------------

   type Enum_State is record
      Initial : Typed.D;
      Count   : Rx_Integer;
      Succ    : Typed.Actions.Func1;
   end record;

   procedure On_Subscribe (Initial  : Enum_State;
                           Observer : in out Typed.Observer)
   is
      use Typed.Conversions;

      Next : Typed.D := Initial.Initial;
   begin
      for I in 1 .. Initial.Count loop
         Observer.On_Next (+Next);
         if I /= Initial.Count then
            Next := +Initial.Succ (+Next);
         end if;
      end loop;
   end On_Subscribe;

   package Enumerate is new With_State (Enum_State, On_Subscribe);

   function Enumerator (Initial : Typed.T;
                        Succ    : not null Typed.Actions.Func1;
                        Count   : Rx_Integer := Rx_Integer'Last) return Typed.Observable is
     (Enumerate.Create (Enum_State'(Typed.Conversions.Def (Initial),
                                    Count,
                                    Succ)));

end Rx.Src.Create;
