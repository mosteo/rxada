package body Rx.Observables is

   ------------
   -- Append --
   ------------

   procedure Append (L : in out Collections.List; V : T) is
   begin
      L.Append (V);
   end Append;

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
