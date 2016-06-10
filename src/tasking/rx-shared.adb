with Ada.Unchecked_Deallocation;

package body Rx.Shared is

   ------------
   -- Create --
   ------------

   function Create (Held : Typed.Consumers.Observer'Class) return Observer is
   begin
      return (Actual => new Typed.Consumers.Observer'Class'(Held));
   end Create;

   -------------
   -- Release --
   -------------

   procedure Release (This : in out Observer) is
      procedure Free is new Ada.Unchecked_Deallocation (Typed.Consumers.Observer'Class, Observer_Access);
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
      This.Actual.On_Next (V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Observer) is
   begin
      This.Actual.On_Completed;
      This.Release;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (This : in out Observer;
      Error : Errors.Occurrence)
   is
   begin
      This.Actual.On_Error (Error);
      This.Release;
   end On_Error;

end Rx.Shared;
