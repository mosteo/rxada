with Rx.Debug;
with Rx.Errors;
with Rx.Impl.Preservers;
with Rx.Op.Funnel;
--  with Rx.Op.Map;
--  with Rx.Op.No_Op;
--  with Rx.Src.Just;
with Rx.Tools.Shared_Data;

package body Rx.Op.Flatmap is

 package Preserver is new Rx.Impl.Preservers (Transformer.Into);

   package RxFunnel is new Rx.Op.Funnel (Preserver);
--     package RxJust   is new Rx.Src.Just (Transformer.From);
--     package RxMap    is new Rx.Op.Map (Transformer);
--     package RxNoop   is new Rx.Op.No_Op (Preserver);

   type Unsafe_Controller is record
      Master_Finished    : Boolean := False;
      Live_Subscriptions : Natural := 0;
   end record;

   procedure Add_Sub (Ctrl : in out Unsafe_Controller) is
   begin
      Ctrl.Live_Subscriptions := Ctrl.Live_Subscriptions + 1;
      Debug.Trace ("flatmap subs++: " & Ctrl.Live_Subscriptions'Img);
   end Add_Sub;

   procedure Del_Sub (Ctrl : in out Unsafe_Controller) is
   begin
      Ctrl.Live_Subscriptions := Ctrl.Live_Subscriptions - 1;
      Debug.Trace ("flatmap subs--: " & Ctrl.Live_Subscriptions'Img);
   end Del_Sub;

   procedure Mark_Completed (Ctrl : in out Unsafe_Controller) is
   begin
      Ctrl.Master_Finished := True;
      Debug.Trace ("flatmap master sub finished");
   end Mark_Completed;

   procedure Mark_Errored (Ctrl : in out Unsafe_Controller) is
   begin
      Ctrl.Master_Finished    := True;
      Ctrl.Live_Subscriptions := 0;
      Debug.Trace ("flatmap finished [on error]");
   end Mark_Errored;

   type Controller_Access is access Unsafe_Controller;

   package Shared_Controllers is new Tools.Shared_Data (Unsafe_Controller,
                                                        Controller_Access);

   type Controller is new Shared_Controllers.Proxy with null record;

   type Front is new Transformer.Operator with record
      Func    : Transformer.Actions.HInflater1;
      Sub2nd  : Boolean := False;
      Control : Controller;
   end record;

   overriding procedure On_Complete (This : in out Front);

   overriding procedure On_Next (This     : in out Front;
                                 V        :        Transformer.From.T);

   overriding procedure Subscribe (This     : in out Front;
                                   Consumer : in out Transformer.Into.Observer'Class);

   type Back is new Preserver.Operator with record
      Pending : Natural := 0; -- Live streams still not completed
      Control : Controller;
   end record;

   overriding procedure On_Complete (This : in out Back);

   overriding procedure On_Error (This : in out Back; E : Errors.Occurrence);

   overriding procedure On_Next (This     : in out Back;
                                 V        :        Preserver.T);


   ------------
   -- Create --
   ------------

   function Create (Func      : Transformer.Actions.TInflater1'Class)
                    return Transformer.Operator'Class
   is
   begin
      return Front'(Transformer.Operator with
                    Func    => Transformer.Actions.Hold (Func),
                    Sub2nd  => <>,
                    Control => <>);
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete (This : in out Front) is
      Done : aliased Boolean := False;
      procedure Check_Done (Ctrl : in out Unsafe_Controller) is
      begin
         Done := Ctrl.Master_Finished and then Ctrl.Live_Subscriptions = 0;
      end Check_Done;
   begin
      Debug.Trace ("front on_complete");
      This.Control.Apply (Mark_Completed'Access);
      This.Control.Apply (Check_Done'Access);
      if Done then
         Debug.Trace ("front on_complete [for real]");
         This.Get_Observer.On_Complete;
         This.Unsubscribe;
      else
         Debug.Trace ("front on_complete [subs pending]");
      end if;
   end On_Complete;

   overriding procedure On_Complete (This : in out Back) is
      Done_Master : aliased Boolean := False;
      Done_Subs   : aliased Boolean := False;
      procedure Check_Done (Ctrl : in out Unsafe_Controller) is
      begin
         Done_Master := Ctrl.Master_Finished;
         Done_Subs   := Ctrl.Live_Subscriptions = 0;
      end Check_Done;
   begin
      Debug.Trace ("back on_complete");

      if not This.Is_Subscribed then
         raise No_Longer_Subscribed;
      end if;

      This.Control.Apply (Check_Done'Access);
      if Done_Master and then Done_Subs then
         --  On_Complete without live subscriptions mean master flow is
         --  complete, and we can pack out
         Debug.Trace ("back on_complete [from front]");
         This.Get_Observer.On_Complete;
         This.Unsubscribe;
      else
         -- No matter if master is complete, being here means we have live subs
         --   and this comes from one of them
         This.Control.Apply (Del_Sub'Access);
         This.Control.Apply (Check_Done'Access);
         if Done_Master and then Done_Subs then
            Debug.Trace ("back on_complete [from subs]");
            This.Get_Observer.On_Complete;
            This.Unsubscribe;
         elsif Done_Master then
            Debug.Trace ("back on_complete [master pending]");
         elsif Done_Subs then
            Debug.Trace ("back on_complete [subs pending]");
         else
            Debug.Trace ("back on_complete [master+subs pending]");
         end if;
      end if;
   end On_Complete;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Back; E : Errors.Occurrence) is
   begin
      if This.Is_Subscribed then
         This.Control.Apply (Mark_Errored'Access);
         This.Get_Observer.On_Error (E);
         This.Unsubscribe;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This     : in out Front;
                                 V        :        Transformer.From.T)
   is
      Observable : Transformer.Into.Observable'Class := This.Func.Cref.Evaluate (V);
      --  Writable copy

--        --  Failed experiments
--        Nop : Preserver.Operator'Class := RxNoop.Create;
--        Map : Transformer.Operator'Class := RxMap.Create (null);
   begin
      Debug.Trace ("front on_next");
      This.Control.Apply (Add_Sub'Access);

--        --  Failed experiments
--        Map.Set_Parent (RxJust.Create (V));
--        Nop.Set_Parent (Map);
--        Nop.Subscribe (This.Get_Observer);

      Observable.Subscribe (This.Get_Observer);
   end On_Next;

   overriding procedure On_Next (This     : in out Back;
                                 V        :        Preserver.T)
   is
   begin
      if This.Is_Subscribed then
         Debug.Trace ("back on_next");
         This.Get_Observer.On_Next (V);
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This     : in out Front;
                                   Consumer : in out Transformer.Into.Observer'Class)
   is
   begin
      if This.Sub2nd = False then
         Debug.Trace ("flatmap subscribe [1st]");
         This.Control := Wrap (new Unsafe_Controller'
                                 (Master_Finished    => <>,
                                  Live_Subscriptions => <>));
         declare
            use Preserver.Linkers;
            Downstream : Preserver.Operator'Class :=
                           Preserver.Operator'Class
                             (RxFunnel.Create
                              & Back'(Preserver.Operator with
                                Pending => <>,
                                Control => This.Control));
         begin
            This.Sub2nd := True;
            Downstream.Set_Parent (This);
            Downstream.Subscribe (Consumer);
         end;
      else
         Debug.Trace ("flatmap subscribe [2nd]");
         Transformer.Operator (This).Subscribe (Consumer);
      end if;
   end Subscribe;

end Rx.Op.Flatmap;
