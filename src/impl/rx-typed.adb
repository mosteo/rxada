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
      Error : Errors.Occurrence;
   begin
      Error.Fill (Except);
      This.On_Error (Error);
      if not Error.Is_Handled then
         Debug.Log ("RxAda saw unhandled error:", Debug.Error);
         Debug.Print (Error.Get_Exception.all);
         Error.Reraise;
      end if;
   end Default_Error_Handler;

end Rx.Typed;
