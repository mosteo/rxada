package body Rx.Op.No_Op is

   type Operator is new Operate.Transform.Operator with null record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T;
                      Child : in out Operate.Transform.Into.Observer)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Next (V);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator is
   begin
      return Operator'(Operate.Transform.Operator with null record);
   end Create;


end Rx.Op.No_Op;
