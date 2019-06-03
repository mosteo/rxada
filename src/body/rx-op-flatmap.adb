with Rx.Debug;
with Rx.Op.Funnel;
with Rx.Impl.Preservers;

package body Rx.Op.Flatmap is

   package Preserver is new Rx.Impl.Preservers (Transformer.Into);

   package RxFunnel is new Rx.Op.Funnel (Preserver);

   type Front is new Transformer.Operator with record
      Func : Transformer.Actions.Flattener1;
      Subs : Natural := 0;
   end record;

   overriding procedure On_Complete (This : in out Front);

   overriding procedure On_Next (This     : in out Front;
                                 V        :        Transformer.From.T);

   overriding procedure Subscribe (This     : in out Front;
                                   Consumer : in out Transformer.Into.Observer'Class);

   type Back is new Preserver.Operator with record
      Pending : Natural := 0;
   end record;

   overriding procedure On_Complete (This : in out Back);

   overriding procedure On_Next (This     : in out Back;
                                 V        :        Preserver.T);


   ------------
   -- Create --
   ------------

   function Create (Func   : Transformer.Actions.Flattener1)
                    return Transformer.Operator'Class
   is
   begin
      Return Front'(Transformer.Operator With
                      Func => Func,
                      Subs => <>);
   end Create;

   -----------------
   -- On_Complete --
   -----------------

   overriding procedure On_Complete (This : in out Front) is
   begin
      Debug.Trace ("front on_complete");
   end On_Complete;

   overriding procedure On_Complete (This : in out Back) is
   begin
      Debug.Trace ("back on_next");
   end On_Complete;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This     : in out Front;
                                 V        :        Transformer.From.T)
   is
      Observable : Transformer.Into.Observable'Class := This.Func (V);
   begin
      Debug.Trace ("front on_next");
      Observable.Subscribe (This.Get_Observer);
   end On_Next;

   overriding procedure On_Next (This     : in out Back;
                                 V        :        Preserver.T)
   is
   begin
      Debug.Trace ("back on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This     : in out Front;
                                   Consumer : in out Transformer.Into.Observer'Class)
   is
   begin
      --  Gets called twice: first with the real downstream, next with our MitM
      if This.Subs /= 0 then
         --  Second subscription call, do nothing
         Debug.Trace ("flatmap 2nd subscribe");
         Transformer.Operator (This).Subscribe (Consumer);
      else
         --  First subscription call with actual downstream
         declare
            use Preserver.Linkers;
            Downstream : Preserver.Operator'Class :=
                           Preserver.Operator'Class
                             (RxFunnel.Create
                              & Back'(Preserver.Operator with
                                      Pending => <>));
         begin
            Debug.Trace ("flatmap 1st subscribe");
            This.Subs := This.Subs + 1;
            Downstream.Set_Parent (This);
            Downstream.Subscribe (Consumer);
         end;
      end if;
   end Subscribe;

end Rx.Op.Flatmap;
