package body Rx.Op.Do_On is

   type Operator is new Preserver.Operator with record
      On_Next : Preserver.Typed.Actions.HTProc1;
   end record;

   overriding procedure On_Next (This : in out Operator;
                                 V    :        Preserver.T);

   ------------
   -- Create --
   ------------

   function Create (On_Next : Preserver.Typed.Actions.TProc1'Class)
                    return Preserver.Operator'Class
   is
      use Preserver.Typed.Actions;
   begin
      return Operator'(Preserver.Operator with On_Next => Hold (On_Next));
   end Create;


   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator;
                                 V    :        Preserver.T) is
   begin
      This.On_Next.Ref.Call (V);
      This.Get_Observer.On_Next (V);
   end On_Next;

end Rx.Op.Do_On;
