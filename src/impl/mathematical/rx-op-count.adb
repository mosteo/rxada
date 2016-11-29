package body Rx.Op.Count is

   use Transform.Into.Conversions;

   type Counter is new Transform.Transformer with record
      Count : Transform.Into.Type_Traits.D;
   end record;

   overriding
   procedure On_Next (This  : in out Counter;
                      V     :        Transform.From.T;
                      Child : in out Transform.Into.Observer'Class);

   overriding
   procedure On_Completed (This : in out Counter;
                          Child : in out Transform.Into.Observer'Class);

   -------------
   -- On_Next --
   -------------

   overriding
   procedure On_Next (This  : in out Counter;
                      V     :        Transform.From.T;
                      Child : in out Transform.Into.Observer'Class)
   is
      pragma Unreferenced (V, Child);
   begin
      This.Count := +Succ (+This.Count);
   end On_Next;


   ------------------
   -- On_Completed --
   ------------------

   overriding
   procedure On_Completed (This : in out Counter;
                          Child : in out Transform.Into.Observer'Class) is
   begin
      Child.On_Next (Transform.Into.Type_Traits.To_Indefinite (This.Count));
      Child.On_Completed;
   end On_Completed;

   -----------
   -- Count --
   -----------

   function Count (First : Transform.Into.T := Default_Initial_Count) return Transform.Transformer'Class
   is
   begin
      return Counter'(Transform.Transformer with Count => +First);
   end Count;

end Rx.Op.Count;
