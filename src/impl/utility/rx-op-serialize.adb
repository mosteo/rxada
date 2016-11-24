with Rx.Errors;
with Rx.Impl.Semaphores;

package body Rx.Op.Serialize is

   subtype Critical_Section is Impl.Semaphores.Critical_Section;

   type Serializer is new Operate.Operator with record
      Mutex : aliased Impl.Semaphores.Shared_Binary;
   end record;

   overriding procedure On_Next (This  : in out Serializer;
                                 V     :        Operate.T;
                                 Child : in out Operate.Observer);



   overriding procedure Unsubscribe (This : in out Serializer);

   overriding procedure Subscribe (Producer : in out Serializer;
                                   Consumer : in out Operate.Subscriber);

   overriding procedure On_Next (This : in out Serializer; V : Operate.T);

   overriding procedure On_Completed (This : in out Serializer);

   overriding procedure On_Error (This : in out Serializer; Error : in out Errors.Occurrence);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Serializer;
                                 V     :        Operate.T;
                                 Child : in out Operate.Observer)
   is
      -- No lock, since it is taken in the non-child parameterized On_Next
   begin
      Child.On_Next (V);
   end On_Next;

   overriding procedure On_Next (This : in out Serializer; V : Operate.T) is
      CS : Critical_Section (This.Mutex'Access);
   begin
      Operate.Operator (This).On_Next (V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Serializer) is
      CS : Critical_Section (This.Mutex'Access);
   begin
      Operate.Operator (This).On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Serializer; Error : in out Errors.Occurrence) is
      CS : Critical_Section (This.Mutex'Access);
   begin
      Operate.Operator (This).On_Error (Error);
   end On_Error;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Serializer) is
      CS : Critical_Section (This.Mutex'Access);
   begin
      Operate.Operator (This).Unsubscribe;
   end Unsubscribe;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (Producer : in out Serializer;
                                   Consumer : in out Operate.Subscriber)
   is
   begin
      Producer.Mutex := Impl.Semaphores.Create;         -- New mutex for this chain
      Operate.Operator (Producer).Subscribe (Consumer); -- Normal subscription
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create return Operate.Preserver is
   begin
      return S : Serializer;
   end Create;

end Rx.Op.Serialize;
