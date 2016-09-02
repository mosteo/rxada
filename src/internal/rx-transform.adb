package body Rx.Transform is

   overriding procedure On_Next (This : in out Operator; V : From.T) is
   begin
      Operator'Class (This).On_Next (V, This.Get_Child);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      This.Get_Child.On_Completed;
      This.Release_Child; -- Not strictly necessary, but frees memory somewhat earlier
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : in out Errors.Occurrence) is
   begin
      This.Get_Child.On_Error (Error); -- Pass it down
      This.Release_Child; -- Not strictly necessary, but frees memory somewhat earlier
   end On_Error;

end Rx.Transform;
