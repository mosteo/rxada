with Ada.Strings.Unbounded;

package body Rx.Observables.Image is

   -----------------------
   -- Addressable_Image --
   -----------------------

   function Addressable_Image (V : T) return String is
   begin
      return Image (V);
   end Addressable_Image;

   ----------------
   -- List_Image --
   ----------------

   function List_Image (L : T_List) return String is
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
   end List_Image;

end Rx.Observables.Image;
