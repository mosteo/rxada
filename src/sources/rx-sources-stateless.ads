with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
   type Initial_State is private;
   with procedure On_Subscribe (State    : Initial_State;
                                Observer : in out Typed.Consumers.Observer'Class) is <>;
package Rx.Sources.Stateless is

--     pragma Preelaborate;

--  Generic observable that can produce all its items from an initial value
--  and hence doesn't need to retain a state going on.
--  The call to On_Complete is PERFORMED BY THIS TYPE, NO NEED TO DO IT IN INSTANCES
   type Observable is new Typed.Producers.Observable with private;

   function Create (State : Initial_State) return Observable'Class;

private

   type Observable is new Typed.Producers.Observable with record
      S : Initial_State;
   end record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Typed.Consumers.Observer'Class);

end Rx.Sources.Stateless;
