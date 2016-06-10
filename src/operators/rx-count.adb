package body Rx.Count is

   --------------------
   -- Discrete_Count --
   --------------------

   type Counter is new Transform.Operator with record
      Count : Transform.Into.Type_Traits.D;
   end record;

   overriding
   procedure On_Completed (This : in out Counter; Child : in out Transform.Into.Observer);

   overriding
   procedure On_Next (This  : in out Counter;
                      Child : in out Transform.Into.Observer;
                      V     : Transform.From.T)
   is
      use Transform.Into.Type_Traits;
   begin
      This.Count := +Succ (+This.Count);
   end On_Next;

   overriding
   procedure On_Completed (This : in out Counter; Child : in out Transform.Into.Observer) is
      use Transform.Into.Type_Traits;
   begin
      Child.On_Next (+This.Count);
      Child.On_Completed;
   end On_Completed;

   function Count (First : Transform.Into.T) return Transform.Operator'Class
   is
      use Transform.Into.Type_Traits;
   begin
      return Counter'(Transform.Operator with Count => +First);
   end Count;

end Rx.Count;
