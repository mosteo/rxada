with Rx.Debug;

package body Rx.Defaults is

   ---------------------------
   -- Default_Error_Handler --
   ---------------------------

   procedure Default_Error_Handler
     (This   : in out Contracts.Observer'Class;
      Except :        Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (Except) = Unimplemented'Identity or else
         Exception_Identity (Except) = Program_Error'Identity or else
         Exception_Identity (Except) = Storage_Error'Identity
      then
         Reraise_Occurrence (Except);
         -- Those are normally not regular exceptions to be dealt by clients
      else
         begin
            This.On_Error (Errors.Create (Except));
         exception
            when E : No_Longer_Subscribed =>
               Debug.Report (E, "On_Error rejected during error handling:", Debug.Impl, Reraise => False);
            when E : others =>
               Debug.Report (E, "Exception during error handling:", Debug.Warn, Reraise => True);
         end;
      end if;
   end Default_Error_Handler;

   ----------------------
   -- Default_On_Error --
   ----------------------

   procedure Default_On_Error (E : Errors.Occurrence) is
   begin
      Debug.Trace ("defaults [on_error]");
      Debug.Report (E.Get_Exception.all, "Unhandled error", Debug.Warn);
      raise Program_Error with "unhandled error";
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
