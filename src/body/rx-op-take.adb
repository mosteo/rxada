package body Rx.Op.Take is

   subtype Parent is Operate.Operator;

   type Operator is new Parent with record
      Pass      : Actions.HTFilter1;
      Emit_Last : Boolean;
   end record;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator; V : Operate.T) is
   begin
      if This.Is_Subscribed then
         if This.Pass.Ref.Check (V) then
            This.Get_Observer.On_Next (V);
         else
            if This.Emit_Last then
               This.Get_Observer.On_Next (V);
            end if;
            This.Get_Observer.On_Complete ;
            This.Unsubscribe;
         end if;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create
     (Pass : Actions.TFilter1'Class;
      Emit_Last : Boolean)
      return Operate.Operator'Class
   is
   begin
      return Operator'(Parent with Pass => Actions.Hold (Pass), Emit_Last => Emit_Last);
   end Create;

   ----------------
   -- Take_Count --
   ----------------

   function Take_Count (Count : Rx_Natural) return Operate.Operator'Class is
   begin
      return Create (Actions.Countdown (Count), Emit_Last => False);
   end Take_Count;

   ----------------
   -- Take_While --
   ----------------

   function Take_While
     (Check : Actions.TFilter1'Class)
      return Operate.Operator'Class
   is
   begin
      return Create (Check, Emit_Last => False);
   end Take_While;

   ----------------
   -- Take_Until --
   ----------------

   function Take_Until
     (Check : Actions.TFilter1'Class)
      return Operate.Operator'Class
   is
      use Actions;
   begin
      return Create (not Check, Emit_Last => True);
   end Take_Until;

   ------------
   -- Create --
   ------------

   function Create (During : Duration) return Operate.Operator'Class is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error with "Unimplemented function Create";
      return Create (During => During);
   end Create;

end Rx.Op.Take;
