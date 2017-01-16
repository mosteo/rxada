package body Rx.Impl.Multisubscribers is

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (This : in out Manager'Class) is
   begin
      This.Subscribed := False;
   end Unsubscribe;


   ------------
   -- Create --
   ------------

   function Create (State : States) return Operator is
   begin
      return Operator'(Transformer.Operator with
                       State => State,
                       others => <>);
   end Create;

   -----------------------
   -- Create_Subscriber --
   -----------------------

   function Create_Subscriber (From : in out Operator'Class) return Subscriber'Class is
   begin
      return Subscriber'(Observable.Contracts.Sink with Manager => From.Manager);
   end Create_Subscriber;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Operator;
      V : Transformer.From.T)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "On_Next unimplemented");
      raise Program_Error with "Unimplemented procedure On_Next";
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "On_Completed unimplemented");
      raise Program_Error with "Unimplemented procedure On_Completed";
   end On_Completed;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (This : in out Operator;
      Observer : in out Transformer.Into.Observer'Class)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Subscribe unimplemented");
      raise Program_Error with "Unimplemented procedure Subscribe";
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

--     overriding procedure Subscribe (This : in out Operator; Consumer : in out Operate.Observer'Class)
--     is
--        Sampler : Sample_Observer;
--        Downstream : Shared.Observer := Shared.Create (Consumer);
--     begin
--        This.Manager    := Shared_Managers.Wrap (new Manager'(Mutex => Impl.Semaphores.Create, others => <>));
--        Sampler.Manager := This.Manager;
--        Tamper (This.Manager).Sampler_Subscription :=
--          Samplers.Contracts.Subscribe (This.Sampler, Subscribers.Create (Sampler));
--        Tamper (This.Manager).Downstream := Downstream;
--        Parent (This).Subscribe (Downstream);
--     end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Unsubscribe unimplemented");
      raise Program_Error with "Unimplemented procedure Unsubscribe";
   end Unsubscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Subscriber;
      V : Observable.T)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "On_Next unimplemented");
      raise Program_Error with "Unimplemented procedure On_Next";
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Subscriber) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "On_Completed unimplemented");
      raise Program_Error with "Unimplemented procedure On_Completed";
   end On_Completed;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Subscriber) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Unsubscribe unimplemented");
      raise Program_Error with "Unimplemented procedure Unsubscribe";
   end Unsubscribe;

end Rx.Impl.Multisubscribers;
