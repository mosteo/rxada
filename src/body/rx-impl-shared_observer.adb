with Ada.Unchecked_Deallocation;

package body Rx.Impl.Shared_Observer is

   ------------
   -- Create --
   ------------

   function Create (Held : Typed.Observer) return Observer is
   begin
      return (Actual => new Typed.Observer'(Held));
   end Create;

   ------------------
   -- Set_Observer --
   ------------------

   procedure Set_Observer (This : in out Observer; Held : Typed.Observer) is
   begin
      if This.Actual /= null then
         raise Constraint_Error with "Shared observer cannot be set again";
      else
         This.Actual := new Typed.Observer'(Held);
      end if;
   end Set_Observer;

   -------------
   -- Release --
   -------------

   procedure Release (This : in out Observer) is
      procedure Free is new Ada.Unchecked_Deallocation (Typed.Observer, Observer_Access);
   begin
      Free (This.Actual);
   end Release;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Observer;
      V : Typed.Type_Traits.T)
   is
   begin
      if This.Actual /= null then
         begin
            This.Actual.On_Next (V);
         exception
            when No_Longer_Subscribed =>
               This.Release;
               raise;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Observer) is
   begin
      if This.Actual /= null then
         This.Actual.On_Complete ;
         This.Release;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (This  : in out Observer;
      Error :        Errors.Occurrence)
   is
   begin
      if This.Actual /= null then
         This.Actual.On_Error (Error);
         This.Release;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Error;

end Rx.Impl.Shared_Observer;
