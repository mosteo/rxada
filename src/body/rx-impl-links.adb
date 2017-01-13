package body Rx.Impl.Links is

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (This : in out Downstream;
      Parent : Typed.Contracts.Observable'Class)
   is
   begin
      This.Parent.Hold (Parent);
   end Set_Parent;

end Rx.Impl.Links;
