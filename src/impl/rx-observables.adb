package body Rx.Observables is

   ---------
   -- "&" --
   ---------

   function "&" (Producer : Observable; Consumer : Sink) return Subscriptions.Subscription
   is
      Actual_L : Observable := Producer;
      Actual_R : Sink       := Consumer;
      --  We create copies to start chain instantiation with fresh links
   begin
      Actual_L.Subscribe (Actual_R);
      return Actual_R.Get_Subscription;
   end "&";

   ------------
   -- Append --
   ------------

   procedure Append (L : in out Collections.List; V : T) is
   begin
      L.Append (V);
   end Append;

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
