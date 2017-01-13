with Rx.Debug;

package body Rx.Defaults is

   ---------------------------
   -- Default_Error_Handler --
   ---------------------------

   procedure Default_Error_Handler
     (This   : in out Contracts.Observer'Class;
      Except :        Ada.Exceptions.Exception_Occurrence)
   is
   begin
      This.On_Error (Errors.Create (Except));
   exception
      when E : others =>
         Debug.Report (E, "Exception during error handling:", Debug.Warn, Reraise => True);
   end Default_Error_Handler;

   ----------------------
   -- Default_On_Error --
   ----------------------

   procedure Default_On_Error (E : Errors.Occurrence) is
   begin
      Debug.Report (E.Get_Exception.all, "Unhandled error", Debug.Warn, Reraise => True);
   end Default_On_Error;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Observer;
                                  E    :        Errors.Occurrence)
   is
      pragma Unreferenced (This);
   begin
      Default_On_Error (E);
   end On_Error;

end Rx.Defaults;
