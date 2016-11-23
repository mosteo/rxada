with Rx.Errors;
with Rx.Impl.Semaphores;

package body Rx.Op.Serialize is

   type Sem_Ptr is access Impl.Semaphores.Binary;

   type Serializer is new Operate.Operator with record
      Mutex : Impl.Semaphores.Shared_Binary;
   end record;

   overriding procedure On_Next (This  : in out Serializer;
                                 V     :        Operate.T;
                                 Child : in out Operate.Observer);
   --  Must always be provided

   overriding procedure On_Completed (This  : in out Serializer;
                                      Child : in out Operate.Observer);
   --  By default calls Child.On_Complete

   overriding procedure On_Error (This  : in out Serializer;
                                  Error : in out Errors.Occurrence;
                                  Child : in out Operate.Observer);

   overriding procedure Unsubscribe (This : in out Serializer);

   overriding procedure Subscribe (Producer : in out Serializer;
                                   Consumer : in out Operate.Subscriber);

   ------------
   -- Create --
   ------------

   function Create return Operate.Preserver is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      raise Program_Error with "Unimplemented function Create";
      return Create;
   end Create;

end Rx.Op.Serialize;
