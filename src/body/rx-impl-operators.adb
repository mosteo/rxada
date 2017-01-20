package body Rx.Impl.Operators is

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Operator) is
   begin
      This.Get_Observer.On_Complete ;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence) is
   begin
      This.Get_Observer.On_Error (Error);
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Operator; Observer : Into.Subscriber'Class) is
   begin
      This.Downstream.Hold (Observer);
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is
   begin
      This.Downstream.Clear;
   end Unsubscribe;

end Rx.Impl.Operators;
