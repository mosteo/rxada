with Ada.Tags;
with Rx.Debug;

package body Rx.Impl.Transformers is

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (This : in out Operator'Class) is

      procedure Print_Upstream (This : Operator'Class) is
      begin
         Debug.Trace ("upst: " & Ada.Tags.Expanded_Name (This'Tag));
         if This.Has_Parent then
            if This.Get_Parent in Operator'Class then
               Print_Upstream (Operator'Class (This.Get_Parent));
            else
               Debug.Trace ("upst: " & Ada.Tags.Expanded_Name (This.Get_Parent'Tag));
               Debug.Trace ("----");
            end if;
         else
            Debug.Trace ("----");
         end if;
      end Print_Upstream;

      procedure Print_Downstream (This : in out Operator'Class) is
      begin
         Debug.Trace ("down: " & Ada.Tags.Expanded_Name (This'Tag));
         if This.Is_Subscribed then
            if This.Get_Observer.Actual.all in Operator'Class then
               Print_Downstream (Operator'Class (This.Get_Observer.Actual.all));
            else
               Debug.Trace ("down: " & Ada.Tags.Expanded_Name (This.Get_Observer.Actual.all'Tag));
               Debug.Trace ("----");
            end if;
         else
            Debug.Trace ("----");
         end if;
      end Print_Downstream;

   begin
      Debug.Trace ("SELF: " & Ada.Tags.Expanded_Name (This'Tag));
      Debug.Trace ("UPSTREAM:");
      Print_Upstream (This);
      Debug.Trace ("DOWNSTREAM:");
      Print_Downstream (This);
   end Debug_Dump;

   ------------------
   -- Get_Observer --
   ------------------

   not overriding function Get_Observer (This : in out Operator) return Into.Holders.Observers.Reference is
      --  This same function, as expression in the spec, bugs out with access checks (???) in 7.3
   begin
      if This.Is_Subscribed then
         return This.Downstream.Ref;
      else
         raise No_Longer_Subscribed;
      end if;
   end Get_Observer;

   ------------------
   -- Set_Observer --
   ------------------

   procedure Set_Observer (This : in out Operator; Consumer : Into.Observer'Class) is
   begin
      if This.Downstream.Is_Empty then
         This.Downstream.Hold (Consumer);
      else
         raise Constraint_Error with "Downstream Observer already set";
      end if;
   end Set_Observer;

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
            This.Set_Observer (Consumer);
            Parent.Subscribe (This);
         end;
      else
         raise Constraint_Error with "Attempting subscription without producer observable";
      end if;
   end Subscribe;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Operator) is
   begin
      This.Get_Observer.On_Complete;
      This.Unsubscribe;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence) is
   begin
      This.Get_Observer.On_Error (Error);
      This.Unsubscribe;
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator; V : From.T) is
   begin
      raise Program_Error with "Must be overriden";
   end On_Next;

   -------------------
   -- Unsubscribe --
   -------------------

   overriding procedure Unsubscribe (This : in out Operator) is
   begin
      This.Downstream.Clear;
   end Unsubscribe;

   ------------------
   -- Concatenate --
   ------------------

   function Concatenate (Producer : From.Observable;
                         Consumer : Operator'Class)
                          return Into.Observable
   is
   begin
      return Actual : Operator'Class := Consumer do
         Actual.Set_Parent (Producer);
      end return;
   end Concatenate;

end Rx.Impl.Transformers;
