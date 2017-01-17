with Rx.Transformers;
with Rx.Typed;

private with Rx.Errors;
private with Rx.Impl.Semaphores;
private with Rx.Impl.Shared_Data;
private with Rx.Impl.Shared_Observer;
private with Rx.Subscriptions;

generic
   with package Transformer is new Rx.Transformers (<>);
   with package Observable  is new Rx.Typed (<>);
   type States is private; -- Some state to be kept between operator creation and subscription
   Thread_Safe : Boolean := True;
   --  Optionally, thread-safety is built-in via mutex
package Rx.Impl.Multisubscribers is

   --  Convenience types for complex operators that can simultaneously subscribe to several observables,
   --  and each of different types.

   --  Everything is built on top of a ref-counted, shared State that's encapsulated in the operator
   --  and subscriber proxies

   type Subscriber (<>) is new Observable.Contracts.Sink with private;

   type Operator (<>) is new Transformer.Operator with private;

   --  Unsubscription of these proxies will cause only their own. In order to cause global unsubscription,
   --  it has to be manually managed through Manager

   type Manager is abstract tagged limited private;
   --  The type to override by implementing operators

   type Manager_Access is access Manager'Class;

   not overriding procedure Subscribe (Man      : in out Manager;
                                       Op       : in out Operator'Class;
                                       State    :        States) is abstract;

   not overriding procedure On_Next (Man      : in out Manager;
                                     Op       : in out Operator'Class;
                                     V        : Transformer.From.T) is abstract;

   not overriding procedure On_Next (Man      : in out Manager;
                                     Sub      : in out Subscriber'Class;
                                     V        :        Observable.T) is abstract;

   not overriding procedure On_Completed (Man      : in out Manager;
                                          Op       : in out Operator'Class) is abstract;

   not overriding procedure On_Completed (Man      : in out Manager;
                                          Sub      : in out Subscriber'Class) is abstract;

   function Create (State : States) return Operator;
   --  The proper operator

   function Create_Subscriber (From : in out Operator'Class) return Subscriber'Class;
   --  Aditional subscribers that can watch other observables

   type Reference (Observer : access Transformer.Into.Observer'Class) is limited null record
     with Implicit_Dereference => Observer;

   function Get_Observer (From : in out Manager) return Reference;
   --  Returns the subscribed observer

   procedure Unsubscribe (This : in out Manager'Class);

   function Is_Subscribed (This : Manager'Class) return Boolean;

   overriding procedure Unsubscribe (This : in out Operator);
   overriding procedure Unsubscribe (This : in out Subscriber);
   --  After calling any of these, the Man parameter in the abstract methods may become invalid
   --  IT IS A BOUNDED ERROR TO USE THAT PARAMETER AFTER CALLING THESE PROCEDURES

private

   package Shared_Observers is new Shared_Observer (Transformer.Into);

   type Manager is abstract tagged limited record
      Mutex      : aliased Impl.Semaphores.Shared_Binary;
      Subscribed : Boolean;
      Downstream : aliased Shared_Observers.Observer;
      --  This could have been a holder but this way we
   end record;

   function Get_Observer (From : in out Manager) return Reference is
     (Reference'(Observer => From.Downstream'Access));

   function Is_Subscribed (This : Manager'Class) return Boolean is (This.Subscribed);

   package Shared_Managers is new Impl.Shared_Data (Manager'Class, Manager_Access);

   type Shared_Manager is new Shared_Managers.Proxy with null record;

   function Tamper is new Shared_Managers.Tamper;

   function Ref (This : Shared_Manager) return Shared_Managers.Ref is (Tamper (Shared_Managers.Proxy (This)));

   type Operator is new Transformer.Operator with record
      State   : States;
      Manager : Shared_Manager;
   end record;

   type Subscriber is new Observable.Contracts.Sink with record
      Manager : Shared_Manager;
   end record;


   overriding procedure On_Next (This : in out Operator; V : Transformer.From.T);

   overriding procedure On_Completed (This : in out Operator);

   overriding procedure On_Error (This : in out Operator; E : Errors.Occurrence);

   overriding function Is_Subscribed (This : Operator) return Boolean is (This.Manager.Is_Valid);

   overriding procedure Subscribe (This : in out Operator; Observer : in out Transformer.Into.Observer'Class);


   overriding procedure On_Next (This : in out Subscriber; V : Observable.T);

   overriding procedure On_Completed (This : in out Subscriber);

   overriding procedure On_Error (This : in out Subscriber; Error : Errors.Occurrence);

   overriding function Is_Subscribed (This : Subscriber) return Boolean is (This.Manager.Is_Valid);

end Rx.Impl.Multisubscribers;
