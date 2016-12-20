with Rx.Debug;
with Rx.Errors;

package body Rx.Typed is

   ---------------------------
   -- Default_Error_Handler --
   ---------------------------

   procedure Default_Error_Handler
     (This   : in out Observer'Class;
      Except : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      This.On_Error (Errors.Create (Except));
   exception
      when E : others =>
         Debug.Report (E, "Exception during error handling:", Debug.Warn, Reraise => True);
   end Default_Error_Handler;

end Rx.Typed;
