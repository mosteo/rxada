with Rx.Impl.Multiobservers;

package body Rx.Op.Merge is

   package Multi is new Impl.Multiobservers (Preserver.Transform, Preserver.Typed);

   subtype Operator   is Multi.Operator;
   subtype Subscriber is Multi.Subscriber;

   type Manager is new Multi.Multiobserver with record
      Subscriptor_Count : Natural := 2;
      Merge_With        : Preserver.Typed.Definite_Observables.Observable;
   end record;

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Operator'Class);

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Operator'Class;
                                 V        : Preserver.T);

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Subscriber'Class;
                                 V        :        Preserver.T);

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Op       : in out Operator'Class);

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Sub      : in out Subscriber'Class);

   ----------------
   -- Remove_One --
   ----------------

   procedure Remove_One (This : in out Manager) is
   begin
      This.Subscriptor_Count := This.Subscriptor_Count - 1;
      if This.Subscriptor_Count = 0 then
         This.Unsubscribe;
         This.Get_Observer.On_Complete ;
      end if;
   end Remove_One;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (Man      : in out Manager;
                                   Op       : in out Operator'Class)
   is
      Consumer : Subscriber'Class := Op.Create_Subscriber;
   begin
      Man.Merge_With.Subscribe (Consumer);
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Op       : in out Operator'Class;
                                 V        : Preserver.T)
   is
      pragma Unreferenced (Op);
   begin
      Man.Get_Observer.On_Next (V);
   end On_Next;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (Man      : in out Manager;
                                 Sub      : in out Subscriber'Class;
                                 V        :        Preserver.T)
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
      Remove_One (Man);
   end On_Complete ;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (Man      : in out Manager;
                                      Sub      : in out Subscriber'Class)
   is
      pragma Unreferenced (Sub);
   begin
      Remove_One (Man);
   end On_Complete ;

   ------------
   -- Create --
   ------------

   function Create
     (Merge_With : Preserver.Observable'Class)
      return Preserver.Operator'Class
   is
   begin
      return Multi.Create_Operator (new Manager'(Multi.Multiobserver with
                             Merge_With => Preserver.Typed.Definite_Observables.From (Merge_With),
                             others     => <>));
   end Create;

end Rx.Op.Merge;