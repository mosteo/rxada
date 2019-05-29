with Rx.Errors;
with Rx.Impl.Shared_Observer;
with Rx.Op.Serialize;

package body Rx.Op.Merge is

   package RxSerialize is new Rx.Op.Serialize (Preserver);

   package Shared_Observer is new Impl.Shared_Observer (Preserver.Typed);

   type Shared_Merger is new Shared_Observer.Observer with null record;

   overriding procedure On_Complete (This : in out Shared_Merger);
   --  Need to override, because it will be correctly called twice

   type Fake_Merger is new Preserver.Operator with record
      Merge_With : Preserver.Typed.Definite_Observables.Observable;
      Shared     : Shared_Merger;
   end record;
   --  Used as a front, during chaining

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T);
   overriding procedure Subscribe (This     : in out Fake_Merger;
                                   Consumer : in out Preserver.Observer'Class);

   type Real_Merger is new Preserver.Operator with record
      Completed  : Natural := 0;
      Debug_Count : Natural := 0;
   end record;
   --  Used as a shared observer, during subscription

   overriding procedure On_Complete  (This : in out Real_Merger);
   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T);

   ------------
   -- Create --
   ------------

   function Create (Merge_With : Preserver.Observable'Class) return Preserver.Operator'Class is
   begin
      return M : Fake_Merger do
         M.Merge_With.From (Merge_With);
      end return;
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete  (This : in out Real_Merger) is
   begin
      if This.Is_Subscribed then
         This.Completed := This.Completed + 1;
      end if;

      if This.Completed = 2 then
         This.Get_Observer.On_Complete;
         This.Unsubscribe;
      end if;
   end On_Complete;

   overriding procedure On_Complete (This : in out Shared_Merger) is
   begin
      This.Ref.On_Complete;
      if This.Is_Valid Then
         if Real_Merger (This.Ref.Actual.all).Completed = 2 then
            This.Release;
         end if;
      end if;

   end On_Complete;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T) is
   begin
      This.Get_Observer.On_Next (V);
   end On_Next;

   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T) is
   begin
      This.Get_Observer.On_Next (V);
      This.Debug_Count := This.Debug_Count + 1;
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (This     : in out Fake_Merger;
                        Consumer : in out Preserver.Observer'Class)
   is
      Real      : Real_Merger;
      Serialize : Preserver.Operator'Class := RxSerialize.Create;
   begin
      Real       .Set_Observer (Consumer);    -- Stores a downstream copy
      This.Shared.Set_Observer (Real);        -- Create a shared front
      Serialize  .Set_Observer (This.Shared); -- Serialize calls to downstream

      --  Set_Observer is the poor man's way of setting chains during
      --    implementation, before all operators and "&" are available.
      --  Also, at this time we do not know if Consumer is Operator or Sink

      This.Merge_With.Subscribe (Serialize);           -- Subscribe to Obs 2
      Preserver.Operator (This).Subscribe (Serialize); -- Subscribe to Obs 1
   end Subscribe;

end Rx.Op.Merge;
