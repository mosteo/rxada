with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Transformers is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Transformer;
      Consumer : in out Into.Subscriber)
   is
--      use type Into.Consumers.Holder;
   begin
      if Producer.Has_Parent then
         declare
            Parent : From.Observable := Producer.Get_Parent; -- Our own copy
         begin
            Producer.Set_Child (Consumer); -- With its own child
            Parent.Subscribe (Producer);
         end;
      else
         raise Constraint_Error with "Attempting subscription without producer observable";
      end if;
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Transformer; V : From.T) is
   begin
      if This.Has_Child then
         Transformer'Class (This).On_Next (V, This.Get_Child);
      else
         raise Subscriptions.No_Longer_Subscribed;
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("Transform.On_Next: caught No_Longer_Subscribed", Debug.Note);
         This.Child.Clear;
         raise;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Transformer) is
   begin
      if This.Has_Child then
         Transformer'Class (This).On_Completed (This.Get_Child);
         This.Child.Clear; -- This implies unsubscription, but does not propagate it down
      else
         raise Subscriptions.No_Longer_Subscribed;
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("Transform.On_Completed: caught No_Longer_Subscribed", Debug.Note);
         This.Child.Clear;
         raise;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Transformer; Error : Errors.Occurrence) is
   begin
      if This.Has_Child then
         declare
            Err : Errors.Occurrence := Error; -- Writable copy, until I fix this mess
         begin
            Transformer'Class (This).On_Error (Err, This.Get_Child); -- Pass it down
            This.Child.Clear;
         exception
            when Subscriptions.No_Longer_Subscribed =>
               Debug.Log ("Transform.On_Error: caught No_Longer_Subscribed", Debug.Note);
               This.Child.Clear;
               raise;
         end;
      else
         Error.Reraise;
      end if;
   end On_Error;

   --  Versions for the descendants to override:

   ------------------
   -- On_Completed --
   ------------------

   procedure On_Completed (This : in out Transformer;
                           Child : in out Into.Observer'Class)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (This  : in out Transformer;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer'Class)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Error  (Error);
   end On_Error;

   -------------------
   -- Unsubscribe --
   -------------------

   overriding
   procedure Unsubscribe (This : in out Transformer) is
   begin
      if This.Is_Subscribed then
         This.Get_Child.Unsubscribe;
         This.Child.Clear;
      end if;
   end Unsubscribe;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (This : in out Transformer; Child : Into.Subscriber) is
   begin
      This.Child.Hold (Child);
   end Set_Child;

   ------------------
   -- Will_Observe --
   ------------------

   function Will_Observe (Producer : From.Observable;
                          Consumer : Transformer'Class)
                          return Into.Observable
   is
   begin
      return Actual : Transformer'Class := Consumer do
         Actual.Set_Parent (Producer);
      end return;
   end Will_Observe;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out New_Operator) is
   begin
      This.Get_Observer.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out New_Operator; Error : Errors.Occurrence) is
   begin
      This.Get_Observer.On_Error (Error);
   end On_Error;

   ------------------
   -- Set_Observer --
   ------------------

   not overriding procedure Set_Observer (This : in out New_Operator; Observer : Into.Subscriber'Class) is
   begin
      This.Observer.Hold (Observer);
   end Set_Observer;

   ------------------
   -- Get_Observer --
   ------------------

   function Get_Observer (This : in out New_Operator'Class) return Into.Holders.Subscribers.Reference is
   begin
      return This.Observer.Ref;
   end Get_Observer;

end Rx.Transformers;
