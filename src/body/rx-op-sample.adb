with Rx.Impl.Multisubscribers;

package body Rx.Op.Sample is

   type Configs is record
      Policy  : Policies;
      Sampler : Samplers.Definite_Observables.Observable;
   end record;

   package Multi is new Impl.Multisubscribers (Operate.Transform, Samplers, Configs, Thread_Safe => True);

   type Manager is new Multi.Manager with record
      Policy  : Policies;
      Value   : Operate.Typed.D;
      Valid   : Boolean := False;
      Ending  : Boolean := False;
   end record;

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Multi.Operator'Class;
                                   Config    :        Configs;
                                   Observer  : in out Operate.Observer'Class);

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Multi.Operator'Class;
                                 V        :        Operate.T;
                                 Observer : in out Operate.Observer'Class);

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Multi.Subscriber'Class;
                                 V        :        Samplers.T;
                                 Observer : in out Operate.Observer'Class);

   overriding procedure On_Completed (Man      : in out Manager;
                                      Op       : in out Multi.Operator'Class;
                                      Observer : in out Operate.Observer'Class);

   overriding procedure On_Completed (Man      : in out Manager;
                                      Sub      : in out Multi.Subscriber'Class;
                                      Observer : in out Operate.Observer'Class);

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Multi.Operator'Class;
                                   Config   :        Configs;
                                   Observer : in out Operate.Observer'Class)
   is
      pragma Unreferenced (Observer);
      Producer : Samplers.Observable'Class := Config.Sampler.To_Indef;
      Consumer : Samplers.Observer'Class   := Op.Create_Subscriber;
   begin
      Man.Policy := Config.Policy;
      Producer.Subscribe (Consumer);
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Multi.Operator'Class;
                                 V        :        Operate.T;
                                 Observer : in out Operate.Observer'Class)
   is
      pragma Unreferenced (Op, Observer);
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
                                 V        :        Samplers.T;
                                 Observer : in out Operate.Observer'Class)
   is
      pragma Unreferenced (Sub, V);
      use Operate.Typed.Conversions;
   begin
      if Man.Valid then
         Observer.On_Next (+ Man.Value);
      end if;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (Man      : in out Manager;
                                      Op       : in out Multi.Operator'Class;
                                      Observer : in out Operate.Observer'Class)
   is
   begin
      if Man.Is_Subscribed then
         Observer.On_Completed;
         Man.Unsubscribe;
      end if;
      Op.Unsubscribe;
   end On_Completed;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (Man      : in out Manager;
                                      Sub      : in out Multi.Subscriber'Class;
                                      Observer : in out Operate.Observer'Class)
   is
   begin
      if Man.Is_Subscribed then
         Observer.On_Completed;
         Man.Unsubscribe;
      end if;
      Sub.Unsubscribe;
   end On_Completed;

   ------------
   -- Create --
   ------------

   function Create
     (Policy  : Policies;
      Sampler : Samplers.Observable'Class)
      return Operate.Operator'Class
   is
   begin
      return Multi.Create ((Policy  => Policy,
                            Sampler => Samplers.Definite_Observables.From (Sampler)));
   end Create;

end Rx.Op.Sample;
