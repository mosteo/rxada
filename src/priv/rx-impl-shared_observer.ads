with Rx.Errors;
with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Impl.Shared_Observer with Preelaborate is

   type Observer is new Typed.Contracts.Observer with private;
   --  In essence this is a carcass for a pointed to observer.
   --  This way, both threads using it access the same actual Observer.
   --  Deallocation is properly done in On_Completed/On_Error

   --  This expects proper serialization of calls, hence is not thread-safe.
   --  See Operator Serialize for a safeguard for cases where this is not true

   function Create (Held : Typed.Observer) return Observer;
   procedure Release (This : in out Observer);

   overriding procedure On_Next      (This : in out Observer; V : Typed.Type_Traits.T);
   overriding procedure On_Completed (This : in out Observer);
   overriding procedure On_Error     (This : in out Observer; Error : Errors.Occurrence);

   Null_Observer : constant Observer;

private

   type Observer_Access is access Typed.Observer;

   type Observer is new Typed.Contracts.Observer with record
      Actual : Observer_Access;
   end record;

   Null_Observer : constant Observer := (Typed.Contracts.Observer with Actual => null);

end Rx.Impl.Shared_Observer;
