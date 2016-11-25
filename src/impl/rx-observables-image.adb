with Ada.Strings.Unbounded;

package body Rx.Observables.Image is

   -----------
   -- Image --
   -----------

   function Image (L : T_List) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String := To_Unbounded_String ("(");
      First  : Boolean := True;
   begin
      for E of L loop
         if not First then
            Append (Result, ", ");
         else
            First := False;
         end if;
         Append (Result, Image (E));
      end loop;

      return To_String (Result & ")");
   end Image;

end Rx.Observables.Image;
