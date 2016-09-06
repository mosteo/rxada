with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Transform is

   overriding procedure Observe
     (Producer : in out Operator;
      Consumer : in out Into.Observer)
   is
--      use type Into.Consumers.Holder;
   begin
      if Producer.Has_Parent then
         declare
            Parent : From.Observable := Producer.Get_Parent; -- Our own copy
         begin
            Producer.Set_Child (Consumer); -- With its own child
            Parent.Observe (Producer);
         end;
      else
         raise Constraint_Error with "Attempting subscription without producer observable";
      end if;
   end Observe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator; V : From.T) is
   begin
      if This.Has_Child then
         Operator'Class (This).On_Next (V, This.Get_Child);
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("Transform.On_Next: caught No_Longer_Subscribed", Debug.Reduced);
         This.Release_Child;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      if This.Has_Child then
         Operator'Class (This).On_Completed (This.Get_Child);
         This.Release_Child; -- Not strictly necessary, but frees memory somewhat earlier
      end if;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : in out Errors.Occurrence) is
   begin
      if This.Has_Child then
         Operator'Class (This).On_Error (Error, This.Get_Child); -- Pass it down
         This.Release_Child; -- Not strictly necessary, but frees memory somewhat earlier
      else
         Error.Reraise;
      end if;
   end On_Error;

   --  Versions for the descendants to override:

   ------------------
   -- On_Completed --
   ------------------

   procedure On_Completed (This : in out Operator;
                           Child : in out Into.Observer'Class)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (This  : in out Operator;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer'Class)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Error  (Error);
   end On_Error;

   -------------------
   -- Release_Child --
   -------------------

   procedure Release_Child (This : in out Operator) is
   begin
      This.Child.Clear;
   end Release_Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (This : in out Operator; Child : Into.Observer) is
   begin
      This.Child.Hold (Child);
   end Set_Child;

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

end Rx.Transform;
