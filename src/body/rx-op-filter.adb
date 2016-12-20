package body Rx.Op.Filter is

   type Operator is new Operate.Implementation.Operator with record
      Func : Operate.Typed.Actions.Filter1;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T)
   is
   begin
      if This.Func (V) then
         This.Get_Subscriber.On_Next (V);
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Filter : not null Operate.Typed.Actions.Filter1) return Operate.Operator'Class is
   begin
      return Operate.Create (Operator'(Operate.Implementation.Operator with Filter));
   end Create;

end Rx.Op.Filter;
