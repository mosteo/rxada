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
      Debug.Trace ("Shared_Observer.On_Complete");
      if Ref (This).Ended then
         raise Program_Error with "Double On_Complete";
      else
         if Ref (This).Checked then
            Ref (This).Ended := True;
         end if;
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
      if Ref (This).Ended then
         raise Program_Error with "Double On_Error";
      else
         if Ref (This).Checked then
            Ref (This).Ended := True;
         end if;
         Ref (This).Actual.Actual.On_Error (Error);
      end if;
   end On_Error;

end Rx.Impl.Shared_Observer;
