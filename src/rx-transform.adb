package body Rx.Transform is

   ----------------
   -- Set_Parent --
   ----------------

   overriding
   procedure Set_Parent
     (This : in out Operator;
      Parent : From.Producers.Observable'Class)
   is
   begin
      This.Parent := From.Producers.To_Holder (Parent);
   end Set_Parent;

   overriding
   function  Get_Parent (This : Operator) return From.Producers.Observable'Class is
   begin
      return This.Parent.Element;
   end Get_Parent;

end Rx.Transform;
