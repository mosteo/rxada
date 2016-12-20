with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Src.Create with Preelaborate is

   --  Three ways of easily creating a new observable for custom emision

   function Parameterless (On_Subscribe : not null access procedure (Observer : in out Typed.Subscriber))
                           return Typed.Observable;
   --  Creates an Observable that requires no parameters, from a function
   --  Does not autocomplete

   type Observable is abstract tagged null record;
   procedure On_Subscribe (This : in out Observable; Observer : in out Typed.Subscriber) is abstract;

   function Tagged_Stateful (Producer : Observable'Class) return Typed.Observable;
   --  Creates an Observable from a tagged type that can store some initial state
   --  Does not autocomplete

   generic
      type State is private;
      with procedure On_Subscribe (Initial  : State;
                                   Observer : in out Typed.Subscriber) is <>;
      Autocompletes : Boolean := True;
      --  Generic observable that can produce all its items from an initial value
      --  and hence doesn't need to retain a state going on.
      --  The call to On_Complete is automatically performed after On_Subscribe if Completes = True
   package With_State is

      function Create (Initial : State) return Typed.Observable;
      --  Creates an observable that can receive different initial states
      --  On_Completed is optionally automatically called

   end With_State;

end Rx.Src.Create;
