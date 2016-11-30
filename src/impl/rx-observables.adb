with Rx.Op.Length;

package body Rx.Observables is

   ------------
   -- Append --
   ------------

   procedure Append (L : in out Collections.List; V : T) is
   begin
      L.Append (V);
   end Append;

   ------------
   -- Length --
   ------------

   function Length return Collections.List_Transformers_Reverse.Operator is
      package RxLength is new Op.Length (Collections.List_Transformers_Reverse, Length);
   begin
      return RxLength.Create;
   end Length;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Producer     : Typed.Observable;
                        On_Next      : Typed.Actions.Proc1   := null;
                        On_Completed : Rx.Actions.Proc0      := null;
                        On_Error     : Rx.Actions.Proc_Error := null)
   is
      S : constant Subscriptions.Subscription := Producer & Subscribe (On_Next, On_Completed, On_Error);
      pragma Unreferenced (S);
   begin
      null; -- Done in the declarative part
   end Subscribe;

end Rx.Observables;
