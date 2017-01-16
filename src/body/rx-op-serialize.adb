with Rx.Errors;
with Rx.Impl.Semaphores;

package body Rx.Op.Serialize is

   subtype Critical_Section is Impl.Semaphores.Critical_Section;

   type Serializer is new Operate.Operator with record
      Mutex : aliased Impl.Semaphores.Shared_Binary;
   end record;

   overriding procedure Unsubscribe (This : in out Serializer);

   overriding procedure Subscribe (Producer : in out Serializer;
                                   Consumer :        Operate.Into.Subscriber'Class);

   overriding procedure On_Next (This : in out Serializer; V : Operate.T);

   overriding procedure On_Completed (This : in out Serializer);

   overriding procedure On_Error (This : in out Serializer; Error : Errors.Occurrence);

   overriding procedure On_Next (This : in out Serializer; V : Operate.T) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      This.Get_Observer.On_Next (V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Serializer) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      This.Get_Observer.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Serializer; Error :        Errors.Occurrence) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      This.Get_Observer.On_Error (Error);
   end On_Error;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Serializer) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      Operate.Operator (This).Unsubscribe;
   end Unsubscribe;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (Producer : in out Serializer;
                                   Consumer :        Operate.Into.Subscriber)
   is
   begin
      Producer.Mutex := Impl.Semaphores.Create;         -- New mutex for this chain
      Operate.Operator (Producer).Subscribe (Consumer); -- Normal subscription
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator'Class is
   begin
      return Serializer'(Operate.Operator with others => <>));
   end Create;

end Rx.Op.Serialize;
