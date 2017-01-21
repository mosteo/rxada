with Rx.Impl.Multiobservers;

package body Rx.Op.Sample is

   package Multi is new Impl.Multiobservers (Operate.Transform, Samplers, Thread_Safe => True);

   type Manager is new Multi.Multiobserver with record
      Policy  : Policies;
      Sampler : Samplers.Definite_Observables.Observable;
      Value   : Operate.Typed.D;
      Valid   : Boolean := False;
   end record;

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Multi.Operator'Class);

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Multi.Operator'Class;
                                 V        :        Operate.T);

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Multi.Subscriber'Class;
                                 V        :        Samplers.T);

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Op       : in out Multi.Operator'Class);

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Sub      : in out Multi.Subscriber'Class);

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Multi.Operator'Class)
   is
      Producer : Samplers.Observable'Class := Man.Sampler.To_Indef;
      Consumer : Samplers.Observer'Class   := Op.Create_Subscriber;
   begin
      Producer.Subscribe (Consumer);
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Multi.Operator'Class;
                                 V        :        Operate.T)
   is
      pragma Unreferenced (Op);
      use Operate.Typed.Conversions;
   begin
      if Man.Policy = Keep_First or else not Man.Valid then
         Man.Value := + V;
         Man.Valid := True;
      end if;
   end On_Next;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Multi.Subscriber'Class;
                                 V        :        Samplers.T)
   is
      pragma Unreferenced (Sub, V);
      use Operate.Typed.Conversions;
   begin
      if Man.Valid then
         Man.Get_Observer.On_Next (+ Man.Value);
      end if;
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Op       : in out Multi.Operator'Class)
   is
   begin
      if Man.Is_Subscribed then
         Man.Get_Observer.On_Complete ;
         Man.Unsubscribe;
      end if;
      Op.Unsubscribe;
   end On_Complete ;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Sub      : in out Multi.Subscriber'Class)
   is
   begin
      if Man.Is_Subscribed then
         Man.Get_Observer.On_Complete ;
         Man.Unsubscribe;
      end if;
      Sub.Unsubscribe;
   end On_Complete ;

   ------------
   -- Create --
   ------------

   function Create
     (Policy  : Policies;
      Sampler : Samplers.Observable'Class)
      return Operate.Operator'Class
   is
   begin
      return Multi.Create_Operator (new Manager'(Multi.Multiobserver with
                           Policy  => Policy,
                           Sampler => Samplers.Definite_Observables.From (Sampler),
                           others  => <>));
   end Create;

end Rx.Op.Sample;