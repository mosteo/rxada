with Rx.Debug;
with Rx.Errors;
with Rx.Op.Funnel;

package body Rx.Op.Merge is

   package RxFunnel    is new Rx.Op.Funnel    (Preserver);

   type Fake_Merger is new Preserver.Operator with record
      Merge_With : Preserver.Typed.Definite_Observables.Observable;
   end record;
   --  Used as a front, during chaining

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T);
   overriding procedure Subscribe (This     : in out Fake_Merger;
                                   Consumer : in out Preserver.Observer'Class);

   type Real_Merger is new Preserver.Operator with record
      Completed  : Natural := 0;
   end record;
   --  Used as a shared observer, during subscription

   overriding procedure On_Complete  (This : in out Real_Merger);
   overriding procedure On_Error (This : in out Real_Merger; E : Errors.Occurrence);
   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T);

   ------------
   -- Create --
   ------------

   function Create (Merge_With : Preserver.Observable'Class;
                    Policy     : Merge_Policies := Rx.Merge)
                    return Preserver.Operator'Class is
   begin
      if Policy /= Rx.Merge then
         raise Unimplemented;
      end if;

      return Preserver.Operator'Class
        (Fake_Merger'(Preserver.Operator with
                      Merge_With => Preserver.Typed.Definite_Observables.From (Merge_With))
         & RxFunnel.Create
         & Real_Merger'(Preserver.Operator with others => <>));
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete  (This : in out Real_Merger) is
   begin
      Debug.Trace ("real_merger on_complete");
      if This.Is_Subscribed then
         This.Completed := This.Completed + 1;
         Debug.Trace ("real_merger on_complete [count]" & This.Completed'Img);

         if This.Completed = 2 then
            This.Get_Observer.On_Complete;
            This.Unsubscribe;
            Debug.Trace ("real_merger on_complete [unsubscribing]" & This.Completed'Img);
         end if;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Complete;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Real_Merger; E : Errors.Occurrence) is
   begin
      Debug.Trace ("real_merger on_error");
      if This.Is_Subscribed then
         This.Completed := 2;
         This.Get_Observer.On_Error (E);
         This.Unsubscribe;
      else
         raise No_Longer_Subscribed;
         --  Might happen due to racing observables upstream
      end if;
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T) is
   begin
      if This.Is_Subscribed then
         Debug.Trace ("fake_merger on_next");
         This.Get_Observer.On_Next (V);
      else
         raise No_Longer_Subscribed; -- On_Error may cause this normally
      end if;
   end On_Next;

   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T) is
   begin
      if This.Is_Subscribed then
         Debug.Trace ("real_merger on_next");
         This.Get_Observer.On_Next (V);
      else
         raise No_Longer_Subscribed; -- On error might cause this
      end if;
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (This     : in out Fake_Merger;
                        Consumer : in out Preserver.Observer'Class)
   is
   begin
      Preserver.Operator (This).Subscribe (Consumer);
      This.Merge_With.Subscribe (Consumer);
   end Subscribe;

end Rx.Op.Merge;
