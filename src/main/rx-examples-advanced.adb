with Rx.Debug; use Rx.Debug;
with Rx.Std;   use Rx.Std;

procedure Rx.Examples.Advanced is
   use Integers;
   use Strings;
--     use String_To_Integer;
   use Integer_To_String;
--     use Numeric.Integers;


begin
   Debug.Put_Line ("Merge example 1 (merged items)");
   Sub :=
     From ((1, 2, 3))
     & Merge (From ((4, 5, 6)))
     & Std.Casts.To_String
     & Subscribe (Debug.Put_Line'Access);

   Debug.Put_Line ("Merge example (merged count)");
   Sub :=
     From ((1, 2, 3))
     & Merge (From ((4, 5, 6)))
     & Numeric.Integers.Count
     & Std.Casts.To_String
     & Subscribe (Debug.Put_Line'Access);

   Debug.Put_Line ("Done.");
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Advanced;
