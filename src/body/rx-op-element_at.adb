package body Rx.Op.Element_At is

   use Operate.Typed.Conversions;

   type Operator (Pos : Rx_Integer) is new Operate.Operator with record
      Has_Default : Boolean := False;
      Default     : Operate.Typed.D;

      Current     : Rx_Integer;
   end record;

   overriding procedure On_Next (This : in out Operator; V : Operate.T);

   overriding procedure On_Complete  (This : in out Operator);

   overriding procedure On_Next (This : in out Operator; V : Operate.T) is
   begin
      if This.Current = This.Pos then
         This.Get_Observer.On_Next (V);
         This.Get_Observer.On_Complete ;
      end if;

      This.Current := This.Current + 1;
   end On_Next;

   overriding procedure On_Complete  (This : in out Operator) is begin
      if This.Current <= This.Pos then
         if not This.Has_Default then
            raise Constraint_Error with "Pos not reached in Element_At";
         else
            This.Get_Observer.On_Next (+ This.Default);
            This.Get_Observer.On_Complete ;
         end if;
      else
         null; -- Otherwise we already completed in On_Next
      end if;
   end On_Complete ;

   ------------
   -- Create --
   ------------

   function Create
     (Pos   : Rx_Integer;
      First : Rx_Integer := 1)
      return Operate.Operator'Class
   is
   begin
      return Operator'(Operate.Operator with
                         Pos         => Pos,
                       Has_Default => False,
                       Default     => <>,
                       Current     => First);
   end Create;

   ----------------
   -- Or_Default --
   ----------------

   function Or_Default
     (Default : Operate.T;
      Pos     : Rx_Integer;
      First   : Rx_Integer := 1)
      return Operate.Operator'Class
   is
   begin
      return Operator'(Operate.Operator with
                         Pos         => Pos,
                       Has_Default => True,
                       Default     => +Default,
                       Current     => First);
   end Or_Default;

end Rx.Op.Element_At;
