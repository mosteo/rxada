with Rx.Debug;
with Rx.Op.Funnel;

package body Rx.Op.Merge is

   package RxFunnel    is new Rx.Op.Funnel    (Preserver);

   type Fake_Merger is new Preserver.Operator with record
      Merge_With : Preserver.Typed.Definite_Observables.Observable;
      Scheduler  : Schedulers.Scheduler;
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
   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T);

   ------------
   -- Create --
   ------------

   function Create (Merge_With : Preserver.Observable'Class;
                    Observe_On : Schedulers.Scheduler := Schedulers.Immediate)
                    return Preserver.Operator'Class is
   begin
      return Preserver.Operator'Class
        (Fake_Merger'(Preserver.Operator with
                      Merge_With => Preserver.Typed.Definite_Observables.From (Merge_With),
                      Scheduler  => Observe_On)
         & RxFunnel.Create
         & Real_Merger'(Preserver.Operator with others => <>));
--        return M : Fake_Merger do
--           M.Merge_With.From (Merge_With);
--           M.Scheduler := Observe_On;
--        end return;
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete  (This : in out Real_Merger) is
   begin
      Debug.Trace ("real_merger no_complete");
      if This.Is_Subscribed then
         This.Completed := This.Completed + 1;

         if This.Completed = 2 then
            This.Get_Observer.On_Complete;
            This.Unsubscribe;
         end if;
         Debug.Trace ("real_merger on_complete count" & This.Completed'Img);
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Complete;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Fake_Merger; V : Preserver.T) is
   begin
      Debug.Trace ("fake_merger on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   overriding procedure On_Next (This : in out Real_Merger; V : Preserver.T) is
   begin
      Debug.Trace ("real_merger on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (This     : in out Fake_Merger;
                        Consumer : in out Preserver.Observer'Class)
   is
   begin
      This.Merge_With.Subscribe (Consumer);
      Preserver.Operator (This).Subscribe (Consumer);
   end Subscribe;

end Rx.Op.Merge;
