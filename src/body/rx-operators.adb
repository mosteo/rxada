with Rx.Debug;
with Rx.Op.Length;

package body Rx.Operators is

   --------------
   -- Diagnose --
   --------------

   procedure Diagnose (This : Into.Observable'Class) is

      procedure Diag_From (This : From.Observable'Class) is
      begin
         if This in From.Operator'Class then
            Debug.Put_Line ("A-A");
            if From.Operator'Class (This).Has_Parent then
               Diag_From (From.Operator'Class (This).Get_Parent);
            end if;
         else
            raise Program_Error with "unexpected class in A";
         end if;
      end Diag_From;

   begin
      if This in Into.Operator'Class then
         Debug.Put_Line ("B-B");
         if Into.Operator'Class (This).Has_Parent then
            Diagnose (Into.Operator'Class (This).Get_Parent);
         end if;
      elsif This in Operator'Class then
         Debug.Put_Line ("A-B");
         if Operator'Class (This).Has_Parent then
            Diag_From (Operator'Class (This).Get_Parent);
         end if;
      else
         raise Program_Error with "unexpected class in B";
      end if;
   end Diagnose;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent (This   : in out Into.Observable'Class;
                         Parent :        From.Observable'Class)
   is
      --  We are in the AB-BB-BB portion of chain.
      --  When we detect the AB, we call Set_Parent_A to continue

      ------------------
      -- Set_Parent_A --
      ------------------
      --  We are now in AA-AA portion of chain
      procedure Set_Parent_A (This   : in out From.Observable'Class)
      is
      begin
         if This in From.Operator'Class then
            declare
               AA : From.Operator'Class renames From.Operator'Class (This);
            begin
               if AA.Has_Parent then
                  Set_Parent_A (AA.Ref_Parent.all);
               else
                  AA.Set_Parent (Parent);
               end if;
            end;
         else
            raise Program_Error with "unexpected class, AA expected";
         end if;
      end Set_Parent_A;

   begin
      if This in Into.Operator'Class then
         --  BB, go upstream
         Set_Parent (Into.Operator'Class (This).Ref_Parent.all, Parent);
      elsif This in Operator'Class then
         declare
            AB : Operator'Class renames Operator'Class (This);
         begin
            if AB.Has_Parent then
               Set_Parent_A (AB.Ref_Parent.all);
            else
               AB.Set_Parent (Parent);
            end if;
         end;
      else
         raise Program_Error with "unexpected class, AB or BB expected";
         -- A is From, B is Into
      end if;
   end Set_Parent;

   ------------
   -- Length --
   ------------

   function Length return Typed_Lists.Operator'Class is
      package RxLength is new Op.Length (Typed_Lists, Length);
   begin
      return RxLength.Create;
   end Length;

   ----------
   -- Size --
   ----------

   function Size return Operator'Class is
      package RxLength is new Op.Length (Typed, Size);
   begin
      return RxLength.Create;
   end Size;

end Rx.Operators;
