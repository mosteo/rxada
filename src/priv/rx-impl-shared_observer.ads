with Rx.Errors;
with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Impl.Shared_Observer with Preelaborate is

   type Observer is new Typed.Contracts.Observer with private;
   --  In essence this is a carcass for a pointed to observer.
   --  This way, both threads using it access the same actual Observer.
   --  Deallocation is properly done in On_Complete /On_Error

   --  This expects proper serialization of calls, hence is not thread-safe.
   --  See Operator Serialize for a safeguard for cases where this is not true

   function Create (Held : Typed.Observer) return Observer;
   procedure Set_Observer (This : in out Observer; Held : Typed.Observer);
   --  For naming consistency with Operator.Set_Observer

   function Is_Valid (This : Observer) return Boolean;
   --  Say if it holds a shared observer still

   procedure Release (This : in out Observer);

   overriding procedure On_Next      (This : in out Observer; V : Typed.Type_Traits.T);
   overriding procedure On_Complete  (This : in out Observer);
   overriding procedure On_Error     (This : in out Observer; Error : Errors.Occurrence);

   Null_Observer : constant Observer;

   type Reference (Actual : access Typed.Observer) is limited null record
     with Implicit_Dereference => Actual;

   function Ref (This : Observer) return Reference;

private

   type Observer_Access is access Typed.Observer;

   type Observer is new Typed.Contracts.Observer with record
      Actual : Observer_Access;
   end record;

   function Is_Valid (This : Observer) return Boolean is (This.Actual /= null);

   function Ref (This : Observer) return Reference is
      (Reference'(Actual => This.Actual));

   Null_Observer : constant Observer := (Typed.Contracts.Observer with Actual => null);

end Rx.Impl.Shared_Observer;
