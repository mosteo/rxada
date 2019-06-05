with Rx.Debug;

package body Rx.Impl.Shared_Observer is

   ---------
   -- Ref --
   ---------

   function Ref (This : in out Observer) return Safe_Observers.Ref is
      function Tamper is new Safe_Observers.Tamper;
   begin
      return Tamper (Safe_Observers.Proxy (This));
   end Ref;

   ------------
   -- Create --
   ------------

   function Create (Held    : Typed.Observer;
                    Checked : Boolean := True) return Observer is
   begin
      Debug.Trace ("shared_observer [create]");
      return Wrap (new Inner_Observer'
                     (Actual  => Definite_Observers.Create (Held),
                      Checked => Checked,
                      Ended   => False));
   end Create;

   ------------------
   -- Is_Completed --
   ------------------

   function Is_Completed (This : Observer) return Boolean is
   begin
      return This.Get.Ended;
   end Is_Completed;

   --------------------
   -- Mark_Completed --
   --------------------

   procedure Mark_Completed (This : in out Observer) is
   begin
      Debug.Trace ("shared_observer [mark_completed]");
      This.Ref.Ended := True;
   end Mark_Completed;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Observer;
      V : Typed.Type_Traits.T)
   is
   begin
      Debug.Trace ("shared_observer on_next");
      Ref (This).Actual.Actual.On_Next (V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Observer) is
   begin
      if Ref (This).Ended and then Ref (This).Checked then
         Debug.Trace ("shared_observer [double on_complete]");
         raise Program_Error with "Double On_Complete";
      else
         Debug.Trace ("shared_observer [on_complete]" & Ref (This).Checked'Img & Ref (This).Ended'Img);
         Ref (This).Ended := True;
         Ref (This).Actual.Actual.On_Complete;
      end if;
   end On_Complete;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (This  : in out Observer;
      Error :        Errors.Occurrence)
   is
   begin
      if Ref (This).Ended and then Ref (This).Checked then
         Debug.Trace ("shared_observer [double on_error]");
         raise Program_Error with "Double On_Error";
      else
         Debug.Trace ("shared_observer [on_error]" & Ref (This).Checked'Img & Ref (This).Ended'Img);
         Ref (This).Ended := True;
         Ref (This).Actual.Actual.On_Error (Error);
      end if;
   end On_Error;

end Rx.Impl.Shared_Observer;
