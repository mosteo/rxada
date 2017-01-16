package body Rx.Transformers is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This : in out Operator; Consumer : in out Into.Observer'Class)
   is
   begin
      if This.Has_Parent then
         declare
            Parent : From.Observable := This.Get_Parent; -- Our own copy
         begin
            This.Downstream.Hold (Consumer);
            Parent.Subscribe (This);
         end;
      else
         raise Constraint_Error with "Attempting subscription without producer observable";
      end if;
   end Subscribe;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      This.Get_Observer.On_Completed;
      This.Unsubscribe;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence) is
   begin
      This.Get_Observer.On_Error (Error);
      This.Unsubscribe;
   end On_Error;

   -------------------
   -- Unsubscribe --
   -------------------

   overriding procedure Unsubscribe (This : in out Operator) is
   begin
      This.Downstream.Clear;
   end Unsubscribe;

   ------------------
   -- Will_Observe --
   ------------------

   function Will_Observe (Producer : From.Observable;
                          Consumer : Operator'Class)
                          return Into.Observable
   is
   begin
      return Actual : Operator'Class := Consumer do
         Actual.Set_Parent (Producer);
      end return;
   end Will_Observe;

end Rx.Transformers;
