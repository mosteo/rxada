package body Rx.Op.Last is

   use Operate.Typed.Conversions;

   type Operator is new Operate.Operator with record
      Filter : Operate.Typed.Actions.HTFilter1;

      Has_Last : Boolean := False;
      Last     : Operate.Typed.D;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T);

   overriding
   procedure On_Complete  (This  : in out Operator);

   -------------
   -- On_Next --
   -------------

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.Typed.T)
   is
   begin
      if This.Filter.Ref.Check (V) then
         This.Last     := + V;
         This.Has_Last := True;
      end if;
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding
   procedure On_Complete  (This  : in out Operator) is
   begin
      if This.Has_Last then
         This.Get_Observer.On_Next (+ This.Last);
         This.Get_Observer.On_Complete ;
      else
         raise Constraint_Error with "Last completed without element";
      end if;
   end On_Complete ;

   ------------
   -- Create --
   ------------

   function Create
     (Check : Operate.Typed.Actions.TFilter1'Class := Operate.Typed.Actions.Always_Pass)
      return Operate.Operator'Class
   is
      use Operate.Typed.Actions;
   begin
      return
        (Operator'(Operate.Operator with
                   Has_Last     => False,
                   Filter       => + Check,
                   others       => <>));
   end Create;

   ----------------
   -- Or_Default --
   ----------------

   function Or_Default
     (Default : Operate.T;
      Check   : Operate.Typed.Actions.TFilter1'Class := Operate.Typed.Actions.Always_Pass)
      return Operate.Operator'Class
   is
      use Operate.Typed.Actions;
   begin
      return
        (Operator'(Operate.Operator with
                   Has_Last     => True,
                   Last         => + Default,
                   Filter       => + Check));
   end Or_Default;

end Rx.Op.Last;
