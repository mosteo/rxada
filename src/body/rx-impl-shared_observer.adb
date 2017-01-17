with Ada.Unchecked_Deallocation;

-- with Gnat.IO; use Gnat.IO;

package body Rx.Impl.Shared_Observer is

   ------------
   -- Create --
   ------------

   function Create (Held : Typed.Observer) return Observer is
   begin
      return (Actual => new Typed.Observer'(Held));
   end Create;

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
         This.Actual.On_Next (V);
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Observer) is
   begin
      if This.Actual /= null then
         This.Actual.On_Completed;
         This.Release;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Completed;

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
