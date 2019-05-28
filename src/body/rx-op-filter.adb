package body Rx.Op.Filter is

   type Operator is new Operate.Operator with record
      Func : Operate.Typed.Actions.HTFilter1;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T)
   is
   begin
      if This.Func.Ref.Check (V) then
         This.Get_Observer.On_Next (V);
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Filter : not null Operate.Typed.Actions.Filter1)
                       return Operate.Operator'Class
   is
      (Create (Operate.Typed.Actions.Wrap (Filter)));

   ------------
   -- Create --
   ------------

   function Create (Filter : Operate.Typed.Actions.TFilter1'Class)
                    return Operate.Operator'Class
   is
      use Operate.Typed.Actions;
   begin
      return Operator'(Operate.Operator with +Filter);
   end Create;

end Rx.Op.Filter;
