with Rx.Subscriptions;

package body Rx.Transform is

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator; V : From.T) is
   begin
      if This.Is_Subscribed then
         Operator'Class (This).On_Next (V, This.Get_Child);
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         This.Unsubscribe;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      if This.Is_Subscribed then
         Operator'Class (This).On_Completed (This.Get_Child);
      end if;
      This.Release_Child; -- Not strictly necessary, but frees memory somewhat earlier
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : in out Errors.Occurrence) is
   begin
      if This.Is_Subscribed then
         Operator'Class (This).On_Error (Error, This.Get_Child); -- Pass it down
      end if;
      This.Release_Child; -- Not strictly necessary, but frees memory somewhat earlier
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

end Rx.Transform;
