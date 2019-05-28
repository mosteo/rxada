with Rx.Debug;
with Rx.Errors;
with Rx.Impl.Shared_Observer;
with Rx.Tools.Semaphores;

package body Rx.Op.Merge is

   package Shared_Observer is new Impl.Shared_Observer (Preserver.Typed);

   subtype Critical_Section is Tools.Semaphores.Critical_Section;

   type Merger is new Preserver.Operator with record
      Mutex      : aliased Tools.Semaphores.Shared;
      Merge_With : Preserver.Typed.Definite_Observables.Observable;
      Observer   : Shared_Observer.Observer;

      Completed  : Natural := 0;
   end record;

   overriding procedure On_Next (This : in out Merger; V : Preserver.T);
   overriding procedure On_Complete  (This : in out Merger);
   overriding procedure On_Error (This : in out Merger; Error : Errors.Occurrence);

   overriding procedure Subscribe (This     : in out Merger;
                                   Consumer : in out Preserver.Observer'Class);

   ------------
   -- Create --
   ------------

   function Create (Merge_With : Preserver.Observable'Class) return Preserver.Operator'Class is
   begin
      return This : Merger do
         This.Merge_With.From (Merge_With);
      end return;
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete  (This : in out Merger) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      if This.Is_Subscribed then
         This.Completed := This.Completed + 1;
      end if;

      Debug.Put_Line ("COMPLETE:" & This.Completed'Img);

      if This.Completed = 2 then
         This.Observer.On_Complete;
         This.Unsubscribe;
      end if;
   end On_Complete;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Merger; V : Preserver.T) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      This.Observer.On_Next (V);
   end On_Next;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This  : in out Merger;
                                  Error :        Errors.Occurrence)
   is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      if This.Is_Subscribed then
         This.Observer.On_Error (Error);
         This.Unsubscribe;
      end if;
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (This     : in out Merger;
                        Consumer : in out Preserver.Observer'Class)
   is
      Actual : Merger := This;
      --  New copy, which is actually shared by both upstream observables
   begin
      --  Create mutex
      Actual.Mutex := Tools.Semaphores.Create_Reentrant;

      --  Store shared observer
      Actual.Observer := Shared_Observer.Create (Consumer);

      declare
         Actual_Shared : Shared_Observer.Observer :=
                           Shared_Observer.Create (Actual);
         --  Because we need both upstream observables to share downstream
      begin
         --  We insert our shared downstream as the regular downstream
         Preserver.Operator (This).Subscribe (Actual_Shared);

         --  Subscribe to the stored 2nd observable
         This.Merge_With.Subscribe (Actual_shared);
      end;
   end Subscribe;

end Rx.Op.Merge;
