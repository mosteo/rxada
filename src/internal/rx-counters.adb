package body Rx.Counters is

   --------------------
   -- Discrete_Count --
   --------------------

   type Counter is new Operators.Typed.Operator with record
      Count : Operators.Into.Typed.Type_Traits.D;
   end record;

   overriding
   procedure On_Completed (This : in out Counter; Child : in out Operators.Into.Typed.Consumers.Observer'Class);

   overriding
   procedure On_Next (This  : in out Counter;
                      Child : in out Operators.Into.Typed.Consumers.Observer'Class;
                      V     : Operators.From.Typed.Type_Traits.T)
   is
      use Operators.Into.Typed.Type_Traits;
   begin
      This.Count := +Succ (+This.Count);
   end On_Next;

   overriding
   procedure On_Completed (This : in out Counter; Child : in out Operators.Into.Typed.Consumers.Observer'Class) is
   begin
      Child.On_Next (Operators.Into.Typed.Type_Traits.To_Indefinite (This.Count));
      Child.On_Completed;
   end On_Completed;

   function Count (First : Operators.Into.Typed.Type_Traits.T) return Operators.Typed.Operator'Class
   is
      use Operators.Into.Typed.Type_Traits;
   begin
      return Counter'(Operators.Typed.Operator with Count => +First);
   end Count;

end Rx.Counters;
