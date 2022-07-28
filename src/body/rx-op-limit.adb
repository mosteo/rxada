with Rx.Debug;

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
      if not This.Completed then
         Debug.Trace ("limit on_complete [completing]");
         This.Completed := True;
         This.Get_Observer.On_Complete ;
         This.Unsubscribe;
      else
         Debug.Trace ("limit on_complete [after completed]");
         --  raise No_Longer_Subscribed;
         --  Not really necessary to raise, since no more calls are expected.
         --  This may happen e.g. with Just (V) & Limit (0)
         --  Limit will complete on the single V and Just.On_Complete will
         --    trigger this case.
         --  This was discovered during tests with Flat_Map.
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
