package body Rx.Op.Length is

   type Transformer is new Transform.Implementation.Operator with null record;

   overriding procedure On_Next (This  : in out Transformer;
                                 V     :        Transform.From.T)
   is
   begin
      This.Get_Subscriber.On_Next (Length (V));
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create return Transform.Operator'Class is
   begin
      return Transform.Create (Transformer'(Transform.Implementation.Operator with null record));
   end Create;

end Rx.Op.Length;
