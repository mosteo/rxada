with Rx.Errors;
with Rx.Impl.Definite_Observer; -- Does not exist yet
with Rx.Impl.Shared_Subscriber;
with Rx.Operate;
with Rx.Shared_Data;

package body Rx.Op.Flatmap is

   package Operate is new Rx.Operate (Typed.Into);

   --  The demiurge will be subscribed as shared_observer to all generated observables
   --  Will have a protected member to track count of observables to which is subscribed
   --  Also to check if a error has occurred, or the wrapper has seen a On_Completed
   --  The wrapper of course also has the shared demiurge stored to pass the complete, error
   --  events, and to wait for completion of the internal sequences in the concat policy
   --  Not sure how the Switch policy can be implemented yet...
   --  Some other intermediator will be probably needed
   --  This mediator will be in the wrapper probably, since it is it who creates the
   --  observables for the demiurge
   --  I knew this fscking flatmap was going to be a handful!

   type Demiurge (Func   : Typed.Actions.Flattener1;
                  Policy : Policies) is new Operate.Operator with
      record
         Child : Impl.Definite_Observer.Observer; -- Does not exist yet
      end record;

   package Shared is new Rx.Impl.Shared_Subscriber (Typed.Into);

   type Wrapper (Func   : Typed.Actions.Flattener1;
                 Policy : Policies) is new Typed.Operator with null record;

   overriding procedure On_Next (This  : in out Wrapper;
                                 V     :        Typed.From.T;
                                 Child : in out Typed.Into.Observer'Class);

   overriding procedure On_Completed (This  : in out Wrapper;
                                      Child : in out Typed.Into.Observer'Class);

   overriding procedure On_Error (This  : in out Wrapper;
                                  Error : in out Errors.Occurrence;
                                  Child : in out Typed.Into.Observer'Class);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Wrapper;
                                 V     :        Typed.From.T;
                                 Child : in out Typed.Into.Observer'Class)
   is
      Obs : Typed.Into.Observable'Class := This.Func (V);
   begin
      null;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This  : in out Wrapper;
                                      Child : in out Typed.Into.Observer'Class)
   is
   begin
      null;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This  : in out Wrapper;
                                  Error : in out Errors.Occurrence;
                                  Child : in out Typed.Into.Observer'Class)
   is
   begin
      null;
   end On_Error;

   -------------
   -- Flatten --
   -------------

   function Flatten
     (Func   : Typed.Actions.Flattener1;
      Policy : Policies)
      return Typed.Transformer
   is
   begin
      return Wrapper'(Typed.Operator with
                        Func   => Func,
                      Policy => Policy);
   end Flatten;

end Rx.Op.Flatmap;
