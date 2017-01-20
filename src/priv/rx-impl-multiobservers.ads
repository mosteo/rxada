with Rx.Transformers;
with Rx.Typed;

private with Rx.Errors;
private with Rx.Impl.Semaphores;
private with Rx.Impl.Shared_Data;
private with Rx.Subscriptions;

generic
   with package Transformer is new Rx.Transformers (<>);
   with package Observable  is new Rx.Typed (<>);
   Thread_Safe : Boolean := True;
   --  Optionally, thread-safety is built-in via mutex
package Rx.Impl.Multiobservers is

   --  Convenience types for complex operators that can simultaneously subscribe to several observables,
   --  and each of different types.

   --  Everything is built on top of a ref-counted, shared Multiobserver that's reachable in the operator
   --  and subscriber proxies

   type Subscriber (<>) is new Observable.Contracts.Sink with private;

   type Operator (<>) is new Transformer.Operator with private;

   --  Unsubscription of these proxies will cause only their own. In order to cause global unsubscription,
   --  it has to be manually managed through Manager

   type Multiobserver is abstract tagged limited private;
   --  The type to override by implementing operators
   --  The shared instance is created at the operator creation time
   --  It is received back in all other methods

   type Manager_Access is access Multiobserver'Class;

   not overriding procedure Subscribe (Man      : in out Multiobserver;
                                       Op       : in out Operator'Class) is abstract;

   not overriding procedure On_Next (Man      : in out Multiobserver;
                                     Op       : in out Operator'Class;
                                     V        : Transformer.From.T) is abstract;

   not overriding procedure On_Next (Man      : in out Multiobserver;
                                     Sub      : in out Subscriber'Class;
                                     V        :        Observable.T) is abstract;

   not overriding procedure On_Complete  (Man      : in out Multiobserver;
                                          Op       : in out Operator'Class) is abstract;

   not overriding procedure On_Complete  (Man      : in out Multiobserver;
                                          Sub      : in out Subscriber'Class) is abstract;

   function Create_Operator (Man   : Manager_access) return Operator;
   --  The proper operator, with its particular manager instance

   function Create_Subscriber (From : in out Operator) return Subscriber'Class;
   --  Aditional subscribers that can watch other observables

   type Reference (Observer : access Transformer.Into.Observer'Class) is limited null record
     with Implicit_Dereference => Observer;

   function Get_Observer (From : in out Multiobserver) return Reference
     with Pre => From.Is_Subscribed or else raise No_Longer_Subscribed;
   --  Returns the subscribed observer

   procedure Unsubscribe (This : in out Multiobserver'Class)
     with Post => not This.Is_Subscribed;

   function Is_Subscribed (This : Multiobserver'Class) return Boolean;

   overriding procedure Unsubscribe (This : in out Operator);
   overriding procedure Unsubscribe (This : in out Subscriber);
   --  NOTE: THESE WON'T UNSUBSCRIBE THE MANAGER, SINCE THAT DEPENDS ON THE OPERATOR SEMANTICS
   --  MANAGER UNSUBSCRIPTION IS THE RESPONSIBILITY OF THE OPERATOR IMPLEMENTATION

private

   type Multiobserver is abstract tagged limited record
      Mutex      : aliased Impl.Semaphores.Shared;
      Subscribed : Boolean := False;
      Downstream : aliased Transformer.Into.Definite_Observers.Observer;
      --  This could have been a holder but this way we
   end record;

   function Is_Subscribed (This : Multiobserver'Class) return Boolean is (This.Subscribed);

   package Shared_Managers is new Impl.Shared_Data (Multiobserver'Class, Manager_Access);

   type Shared_Manager is new Shared_Managers.Proxy with null record;

   function Tamper is new Shared_Managers.Tamper;

   function Ref (This : Shared_Manager) return Shared_Managers.Ref is (Tamper (Shared_Managers.Proxy (This)));

   subtype Operator_Parent is Transformer.Operator;

   type Operator is new Operator_Parent with record
      Manager    : Shared_Manager;
      Subscribed : Boolean := False;
   end record;

   type Subscriber is new Observable.Contracts.Sink with record
      Manager    : Shared_Manager;
      Subscribed : Boolean := False;
   end record;

   overriding function Get_Observer (This : in out Operator) return access Transformer.Into.Observer'Class;

   overriding procedure On_Next (This : in out Operator; V : Transformer.From.T);

   overriding procedure On_Complete  (This : in out Operator);

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence);

   overriding function Is_Subscribed (This : Operator) return Boolean is
     (This.Subscribed and then This.Manager.Is_Valid);

   overriding procedure Subscribe (This : in out Operator; Observer : in out Transformer.Into.Observer'Class);


   overriding procedure On_Next (This : in out Subscriber; V : Observable.T);

   overriding procedure On_Complete  (This : in out Subscriber);

   overriding procedure On_Error (This : in out Subscriber; Error : Errors.Occurrence);

   overriding function Is_Subscribed (This : Subscriber) return Boolean is
     (This.Subscribed and then This.Manager.Is_Valid);

end Rx.Impl.Multiobservers;
