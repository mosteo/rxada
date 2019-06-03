with Rx.Tools.Holders;

package body Rx.Observables is

   ------------
   -- Append --
   ------------

   procedure Append (L : in out Collections.List; V : T) is
   begin
      L.Append (V);
   end Append;

   --------------
   -- Flat_Map --
   --------------

   package Definite_Operators is new Tools.Holders (Operator);

   type Inflater is new Operate.Transform.Actions.TInflater1 with record
      Operator : Definite_Operators.Definite;
   end record;

   overriding function Evaluate (This : Inflater; V : Operate.T) return Operate.Observable is
      (Just (V) & This.Operator.Get);

   function Flat_Map (Pipeline  : Observable;
                      Scheduler : Schedulers.Scheduler   := Schedulers.Immediate) return Operator
   is
   begin
      return RxFlatMap.Create (Inflater'(Operator => Definite_Operators.Hold (Operator (Pipeline))), Scheduler);
   end Flat_Map;

   --------------
   -- For_Each --
   --------------

   procedure For_Each (Producer     : Typed.Observable;
                        On_Next      : Typed.Actions.Proc1   := null;
                        On_Complete  : Rx.Actions.Proc0      := null;
                        On_Error     : Rx.Actions.Proc_Error := null)
   is
      S : constant Subscriptions.Subscription := Producer & Subscribe (On_Next, On_Complete , On_Error);
      pragma Unreferenced (S);
   begin
      null; -- Done in the declarative part
   end For_Each;

   procedure For_Each (Producer : Typed.Observable;
                       Consumer : Typed.Sink)
   is
      S : constant Subscriptions.Subscription := Producer & Consumer
        with Unreferenced;
   begin
      null; -- Done in declarative part
   end For_Each;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (V        : T_List;
                      For_Each : access procedure (V : T)) is
   begin
      for E of V loop
         For_Each (E);
      end loop;
   end Iterate;

end Rx.Observables;
