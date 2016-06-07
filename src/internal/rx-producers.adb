package body Rx.Producers is

   ----------------
   -- Set_Parent --
   ----------------

   overriding procedure Set_Parent
     (This : in out Subscriptor;
      Parent : Observable'Class)
   is
   begin
      This.Parent := To_Holder (Parent);
   end Set_Parent;

   ----------------
   -- Get_Parent --
   ----------------

   overriding function Get_Parent
     (This :        Subscriptor)
      return Observable'Class
   is
   begin
      return This.Parent.Element;
   end Get_Parent;

end Rx.Producers;
