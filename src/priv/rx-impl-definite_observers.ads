with Rx.Contracts;
with Rx.Errors;

private with Rx.Tools.Holders;

generic
   with package Contracts is new Rx.Contracts (<>);
package Rx.Impl.Definite_Observers with Preelaborate is

   type Observer is new Contracts.Observer with private;

   -- A wrapper over the class to use it as definite

   function Create (From : Contracts.Observer'Class) return Observer
     with Post => Create'Result.Is_Valid;

   overriding procedure On_Next      (This : in out Observer; V : Contracts.T)
     with Pre => This.Is_Valid or else raise Constraint_Error;

   overriding procedure On_Complete  (This : in out Observer)
     with Pre => This.Is_Valid or else raise Constraint_Error;

   overriding procedure On_Error     (This : in out Observer; Error : Errors.Occurrence)
     with Pre => This.Is_Valid or else raise Constraint_Error;

   function Is_Valid (This : Observer) return Boolean;

   procedure Clear (This : in out Observer)
     with Post => not This.Is_Valid;

private

   package Holders is new Rx.Tools.Holders (Contracts.Observer'Class);

   type Observer is new Contracts.Observer with record
      Actual : Holders.Definite;
   end record;

end Rx.Impl.Definite_Observers;
