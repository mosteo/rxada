package body Rx.Op.Count is

   use Transform.Into.Conversions;

   type Counter is new Transform.Subscriber with record
      Count : Transform.Into.Type_Traits.D;
   end record;

   overriding
   procedure On_Next (This  : in out Counter;
                      V     :        Transform.From.T);

   overriding
   procedure On_Completed (This : in out Counter);

   -------------
   -- On_Next --
   -------------

   overriding
   procedure On_Next (This  : in out Counter;
                      V     :        Transform.From.T)
   is
      pragma Unreferenced (V);
   begin
      This.Count := +Succ (+This.Count);
   end On_Next;


   ------------------
   -- On_Completed --
   ------------------

   overriding
   procedure On_Completed (This : in out Counter) is
   begin
      This.Get_Subscriber.On_Next (Transform.Into.Type_Traits.To_Indefinite (This.Count));
      This.Get_Subscriber.On_Completed;
   end On_Completed;

   -----------
   -- Count --
   -----------

   function Count (First : Transform.Into.T := Default_Initial_Count) return Transform.Operator'Class
   is
   begin
      return Transform.Create (Counter'(Transform.Subscriber with Count => +First));
   end Count;

end Rx.Op.Count;
