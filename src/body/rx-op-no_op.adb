with Rx.Debug;

package body Rx.Op.No_Op is

   type Operator is new Preserver.Operator with null record;

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Preserver.T);

   ------------
   -- Create --
   ------------

   function Create return Preserver.Operator'Class is
   begin
      return Operator'(Preserver.Operator with null record);
   end Create;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Preserver.T)
   is
   begin
      Debug.Trace ("on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

end Rx.Op.No_Op;
