package body Rx.Op.Distinct is

   type Operator is new Operate.Operator with record
      Prev         : Operate.Typed.D;
      First_Seen   : Boolean := False;
      Are_Distinct : Operate.Typed.Actions.Comparator;
   end record;

   overriding procedure On_Next (This : in out Operator; V : Operate.T) is
      use Operate.Typed.Conversions;
   begin
      if This.First_Seen then
         if This.Are_Distinct (V, + This.Prev) then
            This.Get_Observer.On_Next (V);
            This.Prev := + V;
         end if;
      else
         This.First_Seen := True;
         This.Prev       := + V;
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create
     (Are_Distinct : Operate.Typed.Actions.Comparator := Default_Not_Same'Access)
      return Operate.Operator'Class
   is
   begin
      return Operator'(Operate.Operator with Are_Distinct => Are_Distinct, others => <>);
   end Create;

end Rx.Op.Distinct;
