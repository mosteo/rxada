package body Rx.Op.Length is

   type Transformer is new Transform.Transformer with null record;

   overriding procedure On_Next (This  : in out Transformer;
                                 V     :        Transform.From.T;
                                 Child : in out Transform.Into.Observer)
   is
      pragma Unreferenced (This);
   begin
      Child.On_Next (Length (V));
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Transform.Operator is
   begin
      return T : Transformer;
   end Create;

end Rx.Op.Length;
