--  with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Op.Limit is

   type Operator is new Operate.Operator with record
      Remaining : Rx_Natural;
      Completed : Boolean := False;
   end record;

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Operate.T);

   overriding procedure On_Completed (This  : in out Operator);

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This  : in out Operator) is
   begin
      if not This.Completed then
         This.Completed := True;
         This.Get_Observer.On_Completed;
         This.Unsubscribe;
      end if;
   end On_Completed;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Operate.T)
   is
   begin
      if This.Completed then
         raise No_Longer_Subscribed;
      end if;

      if This.Remaining > 0 then
         This.Get_Observer.On_Next (V);
         This.Remaining := This.Remaining - 1;
      end if;

      if This.Remaining = 0 and not This.Completed then
         This.Completed := True;
         This.Get_Observer.On_Completed;
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
