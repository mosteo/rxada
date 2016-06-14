package body Rx.No_Op is

   type Op is new Operate.Transform.Operator with null record;

   overriding procedure On_Next (This : in out Op; V : Operate.T) is null;

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator is
   begin
      return Op'(Operate.Transform.Operator with null record);
   end Create;

end Rx.No_Op;
