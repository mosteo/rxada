package body Rx.Impl.Operators is

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      This.Get_Subscriber.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence) is
   begin
      This.Get_Subscriber.On_Error (Error);
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
