package body Rx.Observables is

   ------------
   -- Append --
   ------------

   procedure Append (L : in out Collections.List; V : T) is
   begin
      L.Append (V);
   end Append;

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

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (This   : in out Observable'Class;
                         Parent :        Observable'Class)
   is
   begin
      Operator'Class (This).Set_Parent (Parent);
   end Set_Parent;

end Rx.Observables;
