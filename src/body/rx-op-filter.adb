package body Rx.Op.Filter is

   type Operator is new Operate.Preserver with record
      Func : Operate.Typed.Actions.Filter1;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T;
                      Child : in out Operate.Observer'Class)
   is
   begin
      if This.Func (V) then
         Child.On_Next (V);
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Filter : not null Operate.Typed.Actions.Filter1) return Operate.Preserver'Class is
   begin
      return Operator'(Operate.Preserver with Filter);
   end Create;


end Rx.Op.Filter;
