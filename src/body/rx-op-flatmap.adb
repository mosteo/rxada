with Rx.Debug;
with Rx.Errors;
with Rx.Impl.Preservers;
with Rx.Op.Funnel;
--  with Rx.Op.Map;
--  with Rx.Op.No_Op;
with Rx.Src.Just;
with Rx.Tools.Shared_Data;

package body Rx.Op.Flatmap is

 package Preserver is new Rx.Impl.Preservers (Transformer.Into);

   package RxFunnel is new Rx.Op.Funnel (Preserver);
   package RxJust   is new Rx.Src.Just (Transformer.From);
--     package RxMap    is new Rx.Op.Map (Transformer);
--     package RxNoop   is new Rx.Op.No_Op (Preserver);

   type Unsafe_Controller is record
      Subscribed         : Boolean := True;
      Live_Subscriptions : Natural := 1;
      --  One corresponding to the original upstream observable;
      --  In the non-recursive case we get more via elements;
      --  In the recursive case we get even more via the fron resubscribing.
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

   procedure Mark_Errored (Ctrl : in out Unsafe_Controller) is
   begin
      Ctrl.Subscribed         := False;
      Ctrl.Live_Subscriptions := 0;
      Debug.Trace ("flatmap mark_errored");
   end Mark_Errored;

   type Controller_Access is access Unsafe_Controller;

   package Shared_Controllers is new Tools.Shared_Data (Unsafe_Controller,
                                                        Controller_Access);

   type Controller is new Shared_Controllers.Proxy with null record;

   type Front (Use_Chain : Boolean) is new Transformer.Operator with record
      -- Status vars:
      Sub2nd  : Boolean := False;
      Control : Controller;
      Recurse : Boolean := False;

      -- Secondary subscription generators:
      case Use_Chain is
         when True =>
            Chain : Transformer.Into.Definite_Observables.Observable;
         when False =>
            Func  : Transformer.Actions.HInflater1;
      end case;
   end record;

   overriding function Is_Subscribed (This : Front) return Boolean;

   overriding procedure On_Complete (This : in out Front);

   overriding procedure On_Error (This : in out Front; E : Errors.Occurrence);

   overriding procedure On_Next (This     : in out Front;
                                 V        :        Transformer.From.T);

   overriding procedure Subscribe (This     : in out Front;
                                   Consumer : in out Transformer.Into.Observer'Class);

   type Back is new Preserver.Operator with record
      Pending : Natural := 0; -- Live streams still not completed
      Control : Controller;
   end record;

   overriding function Is_Subscribed (This : Back) return Boolean;

   overriding procedure On_Complete (This : in out Back);

   overriding procedure On_Error (This : in out Back; E : Errors.Occurrence);

   overriding procedure On_Next (This     : in out Back;
                                 V        :        Preserver.T);


   ------------
   -- Create --
   ------------

   function Create (Secondary : Transformer.Into.Observable'Class;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class
   is
   begin
      return Front'(Transformer.Operator with
                    Use_Chain => True,
                    Chain     => Transformer.Into.Definite_Observables.From (Secondary),
                    Func      => <>,
                    Sub2nd    => <>,
                    Control   => <>,
                    Recurse   => Recursive);
   end Create;

   function Create (Func      : Transformer.Actions.TInflater1'Class;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class is
   begin
      return Front'(Transformer.Operator with
                    Use_Chain => False,
                    Chain     => <>,
                    Func      => Transformer.Actions.Hold (Func),
                    Sub2nd    => <>,
                    Control   => <>,
                    Recurse   => Recursive);
   end Create;

   -------------------
   -- Is_Subscribed --
   -------------------

   overriding function Is_Subscribed (This : Front) return Boolean is
   begin
      return
        This.Control.Get.Subscribed and then
        Transformer.Operator (This).Is_Subscribed;
   end Is_Subscribed;

   overriding function Is_Subscribed (This : Back) return Boolean is
   begin
      return
        This.Control.Get.Subscribed and then
        Preserver.Operator (This).Is_Subscribed;
   end Is_Subscribed;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete (This : in out Front) is
   begin
      if not This.Is_Subscribed then
         Debug.Trace ("front on_complete [unsubscribed]");
         raise No_Longer_Subscribed;
      end if;

      Debug.Trace ("front on_complete [passing down]");
      -- We don't do checks on completion here, since we can centralize
      --  completion on the downstream unique Back observer
      -- Also, since we don't know if this comes from main upstream or
      --  secondary subscriptions (in the recursive case), we would be unable
      --  to properly do counting.

      This.Get_Observer.On_Complete;
   end On_Complete;

   overriding procedure On_Complete (This : in out Back) is
      Done : aliased Boolean := False;
      procedure Check_Done (Ctrl : in out Unsafe_Controller) is
      begin
         Done := Ctrl.Live_Subscriptions = 0;
      end Check_Done;
   begin
      if not This.Is_Subscribed then
         Debug.Trace ("back on_complete [unsubscribed]");
         raise No_Longer_Subscribed;
      end if;

      This.Control.Apply (Del_Sub'Access);
      This.Control.Apply (Check_Done'Access);
      if Done then
         --  On_Complete without live subscriptions mean all, primary & secondary
         --    flows, have completed
         Debug.Trace ("back on_complete [final]");
         This.Get_Observer.On_Complete;
         This.Unsubscribe;
      else
         Debug.Trace ("back on_complete [subs pending]");
      end if;
   end On_Complete;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Front; E : Errors.Occurrence) is
   begin
      if This.Is_Subscribed then
         Debug.Trace ("flatmap front.on_error [subscribed]");
         This.Get_Observer.On_Error (E);
         This.Control.Apply (Mark_Errored'Access);
         --  Mark after, so Back.On_Error doesn't panic before calling downstream
         This.Unsubscribe;
      else
         Debug.Trace ("flatmap front.on_error [unsubscribed]");
         raise No_Longer_Subscribed;
      end if;
   end On_Error;

   overriding procedure On_Error (This : in out Back; E : Errors.Occurrence) is
   begin
      if This.Is_Subscribed then
         Debug.Trace ("flatmap back.on_error [subscribed]");
         This.Control.Apply (Mark_Errored'Access);
         --  Mark before, so despite what happens downstream we know we finished
         This.Get_Observer.On_Error (E);
         This.Unsubscribe;
      else
         Debug.Trace ("flatmap back.on_error [unsubscribed]");
         raise No_Longer_Subscribed;
      end if;
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This     : in out Front;
                                 V        :        Transformer.From.T)
   is
   begin
      Debug.Trace ("front on_next");

      if This.Is_Subscribed then
         --  Track new subscription
         This.Control.Apply (Add_Sub'Access);

         declare
            function Get_Chain return Transformer.Into.Observable'Class is
            begin
               if This.Use_Chain then
                  return This.Chain.To_Indef;
                  --  A partial chain
               else
                  return This.Func.CRef.Evaluate (V);
                  --  A complete observable chain
               end if;
            end Get_Chain;

            --  The secondary chain to subscribe
            Observable : Transformer.Into.Observable'Class := Get_Chain;
            --  Expression function results in controlledness bug
         begin
            --  Complete source when given partial Chain instead of Inflater
            if This.Use_Chain then
               Set_Parent (Observable, RxJust.Create (V));
               --  Using special cross-type Set_Parent!
            end if;

            if This.Recurse then
               declare
                  RW_Copy : Transformer.Into.Observer'Class := Identity (This);
               begin
                  --  Emit before resubscribing, or otherwise this element will never reach down
                  This.Get_Observer.On_Next (Identity (V));
                  Observable.Subscribe (RW_Copy);
               end;
            else
               Observable.Subscribe (This.Get_Observer);
            end if;
         end;
      end if;
   exception
      when No_Longer_Subscribed =>
         Debug.Trace ("flatmap front.on_next [no_longer_subscribed]");
         raise;
      when E : others =>
         if This.Is_Subscribed then
            Debug.Trace ("flatmap front.on_next [catch -> on_error]");
            Debug.Trace (E, "Caught by flatmap.front.on_next");
            This.Debug_Dump;
            This.On_Error (Errors.Create (E));
            This.Control.Apply (Mark_Errored'Access);
         else
            Debug.Trace ("flatmap front.on_next [catch -> raise]");
            raise;
         end if;
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
                                 (Subscribed         => True,
                                  Live_Subscriptions => 1));
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
