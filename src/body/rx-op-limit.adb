--  with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Op.Limit is

   type Operator is new Operate.Preserver with record
      Remaining : Natural;
      Completed : Boolean := False;
   end record;

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Operate.T;
                                 Child : in out Operate.Observer'Class);

   overriding procedure On_Completed (This  : in out Operator;
                                      Child : in out Operate.Observer'Class);

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This  : in out Operator;
                                      Child : in out Operate.Observer'Class) is
   begin
      if not This.Completed then
         This.Completed := True;
         Child.On_Completed;
      end if;
   end On_Completed;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Operate.T;
                                 Child : in out Operate.Observer'Class)
   is
   begin
--        Debug.Log ("Limit.On_Next", Debug.Warning);

      if This.Remaining > 0 then
         Child.On_Next (V);
         This.Remaining := This.Remaining - 1;
      end if;

      if This.Remaining = 0 and not This.Completed then
         Operator'Class (This).On_Completed; -- This will release the child in Transform
         This.Completed := True;
      else
         raise Subscriptions.No_Longer_Subscribed; -- This will make predecessors desist
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Limit : Natural) return Operate.Preserver'Class is
   begin
      return Operator'(Operate.Preserver with
                         Remaining => Limit,
                         Completed => False);
   end Create;


end Rx.Op.Limit;
