with Rx.Debug;

package body Rx.Impl.Shared_Observer is

   function Ref (This : in out Observer) return Safe_Observers.Ref is
      function Tamper is new Safe_Observers.Tamper;
   begin
      return Tamper (Safe_Observers.Proxy (This));
   end Ref;

   ------------
   -- Create --
   ------------

   function Create (Held : Typed.Observer) return Observer is
   begin
      return Wrap (new Inner_Observer'
                     (Actual => Definite_Observers.Create (Held),
                      Ended  => False));
   end Create;

   ------------------
   -- Is_Completed --
   ------------------

   function Is_Completed (This : Observer) return Boolean is (This.Get.Ended);

   --------------------
   -- Mark_Completed --
   --------------------

   procedure Mark_Completed (This : in out Observer) is
   begin
      Debug.Log ("Shared_Observer manually Mark_Completed", Debug.Note);
      Ref (This).Ended := True;
   end Mark_Completed;

   ------------------
   -- Set_Observer --
   ------------------

   procedure Set_Observer (This : in out Observer; Held : Typed.Observer) is
   begin
      if This.Is_Valid then
         if This.Ref.Actual.Actual.Is_Valid then
            raise Constraint_Error with "Shared observer cannot be set again";
         else
            This.Ref.Actual.Actual := Definite_Observers.Create (Held);
         end if;
      else
         This := Create (Held);
      end if;
   end Set_Observer;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Observer;
      V : Typed.Type_Traits.T)
   is
   begin
      Ref (This).Actual.Actual.On_Next (V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Observer) is
   begin
      Debug.Trace ("Shared_Observer.On_Complete");
      if Ref (This).Ended then
         raise Constraint_Error with "Double On_Complete";
      else
         Ref (This).Ended := True;
         Ref (This).Actual.Actual.On_Complete;
      end if;
   end On_Complete ;

   ------------------------------------
   -- On_Complete_Without_Completion --
   ------------------------------------

   procedure On_Complete_Without_Completion (This : in out Observer) is
   begin
      Debug.Trace ("Shared_Observer.On_Complete_Without_Completion");
      if Ref (This).Ended then
         raise Constraint_Error with "Double On_Complete";
      else
         Ref (This).Actual.Actual.On_Complete;
      end if;
   end On_Complete_Without_Completion;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (This  : in out Observer;
      Error :        Errors.Occurrence)
   is
   begin
      if Ref (This).Ended then
         raise Constraint_Error with "Double On_Error";
      else
         Ref (This).Ended := True;
         Ref (This).Actual.Actual.On_Error (Error);
      end if;
   end On_Error;

end Rx.Impl.Shared_Observer;
