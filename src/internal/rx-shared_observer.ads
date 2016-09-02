with Rx.Errors;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Shared is

--     pragma Preelaborate;

   type Observer is new Typed.Consumers.Observer with private;
   --  In essence this is a carcass for a pointed to observer.
   --  This way, both threads using it access the same actual Observer.
   --  Deallocation is properly done in On_Completed/On_Error

   function Create (Held : Typed.Consumers.Observer'Class) return Observer;
   procedure Release (This : in out Observer);

   overriding procedure On_Next      (This : in out Observer; V : Typed.Type_Traits.T);
   overriding procedure On_Completed (This : in out Observer);
   overriding procedure On_Error     (This : in out Observer; Error : in out Errors.Occurrence);

   Null_Observer : constant Observer;

private

   type Observer_Access is access Typed.Consumers.Observer'Class;

   type Observer is new Typed.Consumers.Observer with record
      Actual : Observer_Access;
   end record;

   Null_Observer : constant Observer := (Typed.Consumers.Observer with Actual => null);

end Rx.Shared;
