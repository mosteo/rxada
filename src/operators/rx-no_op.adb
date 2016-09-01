with Rx.Base_Op;

package body Rx.No_Op is

   procedure On_Next (V     :        Operate.Transform.From.Type_Traits.T;
                      Child : in out Operate.Transform.Typed.Into.Observer'Class) is null;

   package Op is new Rx.Base_Op (Operate.Transform, On_Next);

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator is
   begin
      return Op.Create;
   end Create;

end Rx.No_Op;
