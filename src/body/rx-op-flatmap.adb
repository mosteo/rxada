with Rx.Impl.Multiobservers;

package body Rx.Op.Flatmap is

   package Multi is new Impl.Multiobservers (Transformer, Transformer.Into);
   package From  renames Transformer.From;
   package Into  renames Transformer.Into;

   subtype Operator   is Multi.Operator;
   subtype Subscriber is Multi.Subscriber;

   type Manager (Policy : Policies) is new Multi.Multiobserver with record
      Func              : Transformer.Actions.Flattener1;
      Subscriptor_Count : Natural := 1;
   end record;

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Operator'Class) is null;

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Operator'Class;
                                 V        :        From.T);

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Subscriber'Class;
                                 V        :        Into.T);

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Op       : in out Operator'Class);

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Sub      : in out Subscriber'Class);

   ------------------
   -- Complete_One --
   ------------------

   procedure Complete_One (Man : in out Manager) is
   begin
      Man.Subscriptor_Count := Man.Subscriptor_Count - 1;
      if Man.Subscriptor_Count = 0 then
         Man.Get_Observer.On_Complete ;
      end if;
   end Complete_One;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Operator'Class;
                                 V        :        From.T)
   is
      Consumer : Multi.Subscriber'Class := Op.Create_Subscriber;
      Producer : Into.Observable'Class  := Man.Func (V);
   begin
      Man.Subscriptor_Count := Man.Subscriptor_Count + 1;
      Producer.Subscribe (Consumer);
   end On_Next;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Subscriber'Class;
                                 V        :        Into.T)
   is
      pragma Unreferenced (Sub);
   begin
      Man.Get_Observer.On_Next (V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Op       : in out Operator'Class)
   is
      pragma Unreferenced (Op);
   begin
      Complete_One (Man);
   end On_Complete ;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Sub      : in out Subscriber'Class)
   is
      pragma Unreferenced (Sub);
   begin
      Complete_One (Man);
   end On_Complete ;

   ------------
   -- Create --
   ------------

   function Create (Func   : Transformer.Actions.Flattener1;
                    Policy : Policies := Merge) return Transformer.Operator'Class is
   begin
      if Policy /= Merge then
         raise Program_Error with "Unimplemented";
      end if;

      return Multi.Create_Operator (new Manager'(Multi.Multiobserver with
                           Policy => Policy,
                           Func   => Func,
                           others => <>));
   end Create;

end Rx.Op.Flatmap;
