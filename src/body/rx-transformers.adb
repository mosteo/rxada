with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Transformers is

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Transformer'Class) is
   begin
      if This.Operator.Is_Valid then
         This.Get_Operator.Subscriber.Clear;
         This.Operator.Clear;
      end if;
   end Clear;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Transformer;
      Consumer : in out Into.Subscriber'Class)
   is
   begin
      if Producer.Has_Parent then
         declare
            Parent : From.Observable := Producer.Get_Parent; -- Our own copy
         begin
            Producer.Get_Operator.Set_Subscriber (Consumer);
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
      if This.Operator.Is_Valid then
         This.Get_Operator.On_Next (V);
      else
         raise Subscriptions.No_Longer_Subscribed;
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("Transform.On_Next: caught No_Longer_Subscribed", Debug.Note);
         This.Clear;
         raise;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Transformer) is
   begin
      if This.Operator.Is_Valid then
         begin
            This.Get_Operator.On_Completed;
            This.Clear;
         exception
            when others =>
               This.Clear;
               raise;
         end;
      else
         raise Subscriptions.No_Longer_Subscribed;
      end if;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Transformer; Error : Errors.Occurrence) is
   begin
      if This.Operator.Is_Valid then
         begin
            This.Get_Operator.On_Error (Error);
            This.Clear;
         exception
            when others =>
               This.Clear;
               raise;
         end;
      else
         Error.Reraise;
      end if;
   end On_Error;

   -------------------
   -- Unsubscribe --
   -------------------

   overriding
   procedure Unsubscribe (This : in out Transformer) is
   begin
      if This.Operator.Is_Valid then
         This.Get_Operator.Unsubscribe;
         This.Clear;
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("Transform.Unsubscribe: caught No_Longer_Subscribed", Debug.Note);
         This.Clear;
      when others =>
         This.Clear;
         raise;
   end Unsubscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is
   begin
      if This.Is_Subscribed then
         This.Get_Subscriber.Unsubscribe;
      end if;
      This.Subscriber.Clear;
   end Unsubscribe;

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

   --  Finally, the Operator defaults

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

   ------------------
   -- Set_Observer --
   ------------------

   not overriding procedure Set_Subscriber (This : in out Operator; Observer : Into.Subscriber'Class) is
   begin
      This.Subscriber.Hold (Observer);
   end Set_Subscriber;

end Rx.Transformers;
