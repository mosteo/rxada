with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Op.Limit is

   type Operator is new Operate.Operator with record
      Remaining : Rx_Natural;
      Completed : Boolean := False;
   end record;

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Operate.T);

   overriding procedure On_Complete  (This  : in out Operator);

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This  : in out Operator) is
   begin
      Debug.Trace ("limit on_complete");
      if not This.Completed then
         This.Completed := True;
         This.Get_Observer.On_Complete ;
         This.Unsubscribe;
      else
         raise Program_Error with "Doubly completed";
      end if;
   end On_Complete ;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Operate.T)
   is
   begin
      if This.Completed then
         Debug.Trace ("limit on_next after completed");
         raise No_Longer_Subscribed;
      end if;

      if This.Remaining > 0 then
         Debug.Trace ("limit on_next");
         This.Get_Observer.On_Next (V);
         This.Remaining := This.Remaining - 1;
      end if;

      if This.Remaining = 0 and not This.Completed then
         Debug.Trace ("limit on_next completing");
         This.Completed := True;
         This.Get_Observer.On_Complete ;
         This.Unsubscribe;
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Limit : Rx_Natural) return Operate.Operator'Class is
   begin
      return Operator'(Operate.Operator with
                             Remaining => Limit,
                             Completed => False);
   end Create;


end Rx.Op.Limit;
