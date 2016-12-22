package body Rx.Op.No_Op is

   type Operator is new Operate.Operator with null record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T)
   is
   begin
      This.Get_Subscriber.On_Next (V);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator'Class is
   begin
      return Operator'(Operate.Operator with null record);
   end Create;


end Rx.Op.No_Op;
