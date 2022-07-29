with Rx.Actions.Transform;
with Rx.Errors;
with Rx.Impl.Links;
with Rx.Impl.Typed;

generic
   with package From2 is new Rx.Impl.Typed (<>);
   with package Into2 is new Rx.Impl.Typed (<>);
package Rx.Impl.Transformers with Preelaborate is

   package From renames From2; -- Visibility bug workaround
   package Into renames Into2; 
   
   function Broken_Identity (Unused : From.T) return Into.T is
     (raise Program_Error with "Incompatible conversion");
   
   --  Renamings for bug workarounds
   subtype From_Observable is From.Observable'Class;
   subtype Into_Observable is Into.Observable'Class;

   package Actions is new Rx.Actions.Transform (From.Contracts, Into.Contracts);

   --  Transformative operator scaffolding:
   package Links is new Rx.Impl.Links (From);

   type Operator is new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Observer and
     From.Contracts.subscriber
   with private;
   --  This is the fundamental type that bridges observables y observers doing something along the way
   --  Override the Observer/Subscriber inherited methods in new operators

   overriding procedure On_Next (This : in out Operator; V : From.T);
   --  Must be overriden to transform From.T --> Into.T
   --  By default, it raises Program_Error

   overriding procedure On_Complete  (This : in out Operator);
   --  By default calls downstream On_Complete 

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence);
   --  By default calls downstream On_Error

   overriding function Is_Subscribed (This : Operator) return Boolean;

   overriding procedure Subscribe (This : in out Operator; Consumer : in out Into.Observer'Class);
--     with Post'Class => This.Is_Subscribed;
   --  Can be overriden to modify the actual consumer that will be stored.
   --  In that case, the parent implementation should be called  
   
   --  Typically, there won't be a need to override these:   

   overriding procedure Unsubscribe (This : in out Operator);

   not overriding function Get_Observer (This : in out Operator) 
                                         return Into.Holders.Observers.Reference;

   ---------------------
   --  Chain building --
   --------------------- 

   function Concatenate (Producer : From.Observable;
                          Consumer : Operator'Class)
                          return Into.Observable;
   --  This does the magic of preparing a passive chain, ready for actual subscription/observation
   
   procedure Debug_Dump (This : in out Operator'Class);
   --  Print upstream/downstream chains

   function "&" (Producer : From.Observable;
                 Consumer : Operator'Class)
                 return Into.Observable renames Concatenate;
   
   --  Useable package
   package Linkers is
      function "&" (Producer : From.Observable;
                    Consumer : Operator'Class)
                    return Into.Observable renames Transformers."&";
   end Linkers;

private

   type Operator is new
     Links.Downstream and
     Into.Contracts.Observable and
     From.Contracts.Observer and
     From.Contracts.Subscriber
   with record
      Downstream : Into.Holders.Observer;
   end record;
   
   procedure Set_Observer (This : in out Operator; Consumer : Into.Observer'Class);
   --  To be used internally here. Not to be used as a way to bypass Subscribe,
   --    since this can cause missing initializations in operators that override
   --    Subscribe.
   --  When needing custom chaining in an operator (e.g. Merge), do it with
   --    Concatenate or Set_Parent, and subscribe normally
   
   overriding function Is_Subscribed (This : Operator) return Boolean is 
     (This.Downstream.Is_Valid);

end Rx.Impl.Transformers;
