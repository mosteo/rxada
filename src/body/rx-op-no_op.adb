package body Rx.Op.No_Op is

   type Operator is new Operate.Transform.New_Operator with null record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T)
   is
   begin
      This.Get_Observer.On_Next (V);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Operate.Preserver'Class is
   begin
      return Operate.Create (Operator'(null record));
   end Create;


end Rx.Op.No_Op;
