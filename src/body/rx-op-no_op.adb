package body Rx.Op.No_Op is

   type Operator is new Operate.Preserver with null record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T;
                      Child : in out Operate.Observer'Class)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Next (V);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Operate.Preserver'Class is
   begin
      return Operator'(Operate.Preserver with null record);
   end Create;


end Rx.Op.No_Op;
