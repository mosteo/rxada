with Rx.Debug;

package body Rx.Impl.Links is

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (This : in out Downstream;
      Parent : Typed.Contracts.Observable'Class)
   is
   begin
      --  If operator already has a parent, this means it belongs to a
      --    partial chain. We must propagate the parenting up to the first
      --    element in the chain.
      if This.Parent.Is_Empty then
         This.Parent.Hold (Parent);
      else
         --  Untested...
         if This.Parent.CRef.Actual.all in Downstream then
            declare
               Current_Parent : Downstream'Class Renames
                                  Downstream'Class (This.Parent.Ref.Actual.all);
            begin
               Debug.Put_Line ("Parenting upstream");
               Current_Parent.Set_Parent (Parent);
            end;
         else
            raise Program_Error with "Unexpected unchainable found upstream";
         end if;
      end if;
   end Set_Parent;

end Rx.Impl.Links;
