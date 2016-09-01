with Rx.Base_Op;

package body Rx.No_Op is

   -------------
   -- On_Next --
   -------------

   procedure On_Next (V     :        Operate.T;
                      Child : in out Operate.Observer) is
   begin
      Child.On_Next (V);
   end On_Next;

   package Operator is new Rx.Base_Op (Operate.Transform, On_Next);

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator renames Operator.Create;


end Rx.No_Op;
