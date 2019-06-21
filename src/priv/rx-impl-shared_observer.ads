with Rx.Errors;
with Rx.Impl.Typed;

private with Rx.Impl.Definite_Observers;
private with Rx.Tools.Shared_Data;

generic
   with package Typed is new Rx.Impl.Typed (<>);
package Rx.Impl.Shared_Observer with Preelaborate is

   type Observer is new Typed.Contracts.Observer with private;
   --  In essence this is a carcass for a pointed to observer.
   --  This way, both threads using it access the same actual Observer.
   --  Deallocation is properly done in On_Complete /On_Error

   --  Thread-safe

   function Create (Held    : Typed.Observer;
                    Checked : Boolean := True) return Observer;
   --  If checked, then only On_Complete/On_Error is allowed
   --  No check is performed otherwise (useful in e.g. Merge/Funnel)

   overriding procedure On_Next      (This : in out Observer; V : Typed.Type_Traits.T);
   overriding procedure On_Complete  (This : in out Observer);
   overriding procedure On_Error     (This : in out Observer; Error : Errors.Occurrence);

   function Is_Completed (This : Observer) return Boolean;

   procedure Mark_Completed (This : in out Observer);

private

   --  The conceptually simple initial design, alas, cannot be: when subscrip-
   --    tions end downstream, and a remote operator is in mid-chain (e.g.,
   --    Observe_On), there's no simple way to inform upstream, that has copies
   --    of the same Shared_Observer. Thus, we have to keep track of how many
   --    copies of an observer still remain.
   --  Furthermore, since an observed gets stored in different threads (e.g.
   --    when Observe_On/Interval is involved), we need full thread-safety.

   package Definite_Observers is new Impl.Definite_Observers (Typed.Contracts);

   type Inner_Observer is limited record
      Actual  : Definite_Observers.Observer;
      Checked : Boolean := True;
      Ended   : Boolean := False;
   end record;

   type Inner_Observer_Access is access Inner_Observer;

   package Safe_Observers is new Tools.Shared_Data (Inner_Observer,
                                                    Inner_Observer_Access,
                                                    "shared_observer");

   type Observer is new Safe_Observers.Proxy and Typed.Contracts.Observer
   with null record;

   function Ref (This : in out Observer) return Safe_Observers.Ref;

end Rx.Impl.Shared_Observer;
