with Rx.Errors;
--  with Rx.Impl.Definite_Observer; -- Does not exist yet
with Rx.Impl.Shared_Observer;
with Rx.Preserve;
with Rx.Subscriptions;

package body Rx.Op.Flatmap is

   package Operate is new Rx.Preserve (Typed.Into);
   package Shared  is new Rx.Impl.Shared_Observer (Typed.Into);

   --  The demiurge will be subscribed as shared_observer to all generated observables
   --  Will have a protected member to track count of observables to which is subscribed
   --  Also to check if a error has occurred, or the wrapper has seen a On_Completed
   --  The wrapper of course also has the shared demiurge stored to pass the complete, error
   --  events, and to wait for completion of the internal sequences in the concat policy
   --  Not sure how the Switch policy can be implemented yet...
   --  Some other intermediator will be probably needed
   --  This mediator will be in the wrapper probably, since it is it who creates the
   --  observables for the demiurge

   --  Now, since we'll possibly have multiple threads competing of On_Next, Unsubscribe,
   --  this will require that the mediator be thread safe. The mediator could be a simple No_Op?
   --  IIUC, this serialization is not needed in the Concat case

   --  I knew this fscking flatmap was going to be a handful!

   type Demiurge;

   protected type Safe (Parent : access Demiurge) is
      private
   end Safe;

   protected body Safe is
   end Safe;

   ----------------
   --  Demiurge  --
   ----------------

   type Demiurge is new Operate.Operator with record
      Child            : Shared.Subscriber;
      Parent_Completed : Boolean with Atomic;
   end record;

   overriding procedure On_Next (This  : in out Demiurge;
                                 V     :        Typed.Into.T);

   overriding procedure On_Next (This  : in out Demiurge;
                                 V     :        Typed.Into.T;
                                 Child : in out Typed.Into.Observer'Class);

   procedure Mark_Completed (This : in out Demiurge) is
   begin
      This.Parent_Completed := True;
   end Mark_Completed;

   overriding procedure On_Next (This  : in out Demiurge;
                                 V     :        Typed.Into.T)
   is
   begin
      This.This.Get_Subscriber.On_Next (V);
   end On_Next;

   overriding procedure On_Next (This  : in out Demiurge;
                                 V     :        Typed.Into.T;
                                 Child : in out Typed.Into.Observer'Class) is
   begin
      raise Program_Error with "Should never be called";
   end On_Next;

   procedure Set_Child (This : in out Demiurge) is
   begin
      This.Child := Shared.Create (This);
   end Set_Child;

   ---------------
   --  Wrapper  --
   ---------------

   type Wrapper (Func   : Typed.Actions.Flattener1;
                 Policy : Policies) is new Typed.Operator with
      record
         Child   : Demiurge;
         Current : Subscriptions.Subscription; -- To abort current in switch mode
      end record;

   overriding procedure On_Next (This  : in out Wrapper;
                                 V     :        Typed.From.T;
                                 Child : in out Typed.Into.Observer'Class);

   overriding procedure On_Completed (This  : in out Wrapper;
                                      Child : in out Typed.Into.Observer'Class);

   overriding procedure On_Error (This  : in out Wrapper;
                                  Error :        Errors.Occurrence;
                                  Child : in out Typed.Into.Observer'Class);

   overriding procedure Subscribe (Producer : in out Wrapper;
                                   Consumer : in out Typed.Into.Subscriber);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Wrapper;
                                 V     :        Typed.From.T;
                                 Child : in out Typed.Into.Observer'Class)
   is
      pragma Unreferenced (Child); -- Note that this is not the proper child, besides
      Obs : Typed.Into.Observable'Class := This.Func (V);
   begin
      Obs.Subscribe (This.Child);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This  : in out Wrapper;
                                      Child : in out Typed.Into.Observer'Class)
   is
      pragma Unreferenced (Child); -- Note that this is not the proper child, besides
   begin
      Mark_Completed (This.Child);
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This  : in out Wrapper;
                                  Error :        Errors.Occurrence;
                                  Child : in out Typed.Into.Observer'Class)
   is
      pragma Unreferenced (Child); -- Note that this is not the proper child, besides
   begin
      This.Child.On_Error (Error);
   end On_Error;

   overriding procedure Subscribe (Producer : in out Wrapper;
                                   Consumer : in out Typed.Into.Subscriber)
   is
   begin
      Producer.Child := Shared.Create (Consumer);
      Typed.Operator (Producer).Subscribe (Producer.Child);
   end Subscribe;

   -------------
   -- Flatten --
   -------------

   function Flatten
     (Func   : Typed.Actions.Flattener1;
      Policy : Policies)
      return Typed.Operator
   is
   begin
      return Wrapper'(Typed.Operator with
                      Func    => Func,
                      Policy  => Policy,
                      Child   => <>,
                      Current => <>);
   end Flatten;

end Rx.Op.Flatmap;
