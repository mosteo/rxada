package body Rx.Op.Scan is

   use Typed.Into.Conversions;

   type Operator is new Typed.Operator with record
      Func : Typed.Actions.Func2;
      Acum : Typed.Into.D;
      Emit : Boolean;
   end record;

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Typed.Into.Observer'Class);

   overriding
   procedure On_Next (This  : in out Operator;
                      V     : Typed.From.T);

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Typed.Into.Observer'Class)
   is
   begin
      Typed.Operator (Producer).Subscribe (Consumer);
      if Producer.Emit then
         Producer.Get_Observer.On_Next (+ Producer.Acum);
      end if;
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding
   procedure On_Next (This  : in out Operator;
                      V     : Typed.From.T)
   is
   begin
      This.Acum := + This.Func (V, + This.Acum);
      This.Get_Observer.On_Next (+ This.Acum);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create
     (Func : Typed.Actions.Func2;
      Seed : Typed.Into.T;
      Emit : Boolean      := False)
      return Typed.Operator'Class
   is
   begin
      return (Operator'(Typed.Operator with
                       Func => Func,
                       Acum => + Seed,
                       Emit => Emit));
   end Create;

end Rx.Op.Scan;
