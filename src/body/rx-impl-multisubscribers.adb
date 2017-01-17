package body Rx.Impl.Multisubscribers is

   subtype Critical_Section is Impl.Semaphores.Critical_Section;

   ------------------
   -- Get_Observer --
   ------------------

   function Get_Observer (From : in out Manager) return Reference is
     (Reference'(Observer => From.Downstream'Access));

   -----------------
   -- Unsubscribe --
   -----------------

   procedure Unsubscribe (This : in out Manager'Class) is
   begin
      This.Subscribed := False;
      This.Downstream.Clear;
   end Unsubscribe;

   ------------
   -- Create --
   ------------

   function Create (Man   : Manager_access) return Operator is
   begin
      return Operator'(Transformer.Operator with
                       Manager => Wrap (Man),
                       others => <>);
   end Create;

   -----------------------
   -- Create_Subscriber --
   -----------------------

   function Create_Subscriber (From : in out Operator'Class) return Subscriber'Class is
   begin
      return Subscriber'(Observable.Contracts.Sink with Manager => From.Manager, Subscribed => True);
   end Create_Subscriber;

   ------------------
   -- Get_Observer --
   ------------------

   overriding function Get_Observer (This : in out Operator) return access Transformer.Into.Observer'Class is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
         begin
            return Man.Downstream'Unchecked_Access;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end Get_Observer;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Operator;
      V : Transformer.From.T)
   is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
            CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         begin
            if Man.Subscribed then
               Man.On_Next (This, V);
            else
               raise No_Longer_Subscribed;
            end if;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Operator) is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
            CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         begin
            if Man.Subscribed then
               Man.On_Completed (This);
               This.Unsubscribe;
            else
               raise No_Longer_Subscribed;
            end if;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error : Errors.Occurrence) is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
            CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         begin
            if Man.Subscribed then
               Man.Downstream.On_Error (Error);
               This.Unsubscribe;
               Man.Unsubscribe;
            else
               raise No_Longer_Subscribed;
            end if;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (This     : in out Operator;
      Observer : in out Transformer.Into.Observer'Class)
   is
      Man : Manager'Class renames This.Manager.Ref;
   begin
      This.Subscribed := True;
      Man.Mutex := Impl.Semaphores.Create (not Thread_Safe);
      declare
         CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         --  Needed because if a subscription is performed during operator subscription it could race us
         Parent : Transformer.From.Observable := This.Get_Parent;
      begin
         Man.Downstream := Transformer.Into.Definite_Observers.Create (Observer);
         Parent.Subscribe (This);
         Man.Subscribe (This);
      end;
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is
   begin
      This.Subscribed := False;
      This.Manager.Forget;
   end Unsubscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Subscriber;
      V : Observable.T)
   is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
            CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         begin
            if Man.Subscribed then
               Man.On_Next (This, V);
            else
               raise No_Longer_Subscribed;
            end if;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Subscriber) is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
            CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         begin
            if Man.Subscribed then
               Man.On_Completed (This);
               This.Unsubscribe;
            else
               raise No_Longer_Subscribed;
            end if;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Subscriber; Error : Errors.Occurrence) is
   begin
      if This.Manager.Is_Valid then
         declare
            Man : Manager'Class renames This.Manager.Ref;
            CS  : Critical_Section (Man.Mutex'Access) with Unreferenced;
         begin
            if Man.Subscribed then
               Man.Downstream.On_Error (Error);
               This.Unsubscribe;
               Man.Unsubscribe;
            else
               raise No_Longer_Subscribed;
            end if;
         end;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Error;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Subscriber) is
   begin
      This.Subscribed := False;
      This.Manager.Forget;
   end Unsubscribe;

end Rx.Impl.Multisubscribers;
