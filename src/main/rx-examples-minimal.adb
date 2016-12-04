with Rx.Debug;
with Rx.Std; use Rx.Std;

--  This procedure tests unambiguous visibility of "&"

procedure Rx.Examples.Minimal is

   use Integers;
   use Strings;

--     use Chars.Observables;
--     use Intarrs.Observables;

   use String_To_Integer;
   use Integer_To_String;
   use Numeric.Integers;
   use Numeric.Str_To_Int;

begin
   Sub :=
     Just ("Hello, world!")
     & -- Check subscription alone
     Subscribe (Debug.Put_Line'Access);

   Nosub :=
     -
     (
      Just ("Hello, world!")
      & -- Check single type-preserving operation alone;
      No_Op
     );

   Nosub :=
     -
     (
      Just ("Hello, world!")
      & -- Check multiple type-preserving operation alone;
        No_Op
      &
        No_op
     );

   Sub := -- Check type-preserving op plus subscription
     Just ("Hello, world!")
     &
     No_Op
     &
     Subscribe (Debug.Put_Line'Access);

   Nosub :=
     -
     (
      Just ("Hello, world!")
      & -- Check transformation alone;
        Map (Length'Access)
     );

   Nosub := -- Check roundtrip
     -
     (
      Just ("Hello, world!")
      &
        Map (Length'Access)
      &
        Map (Image'Access)
     );

   Sub := -- Check roundtrip plus subscription
     (
      Just ("Hello, world!")
      &
        Map (Length'Access)
      &
        Map (Image'Access)
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Nosub := -- Check counting of same type without subscription
     -
     (
      Just (0)
      &
        Count (First => 0)
     );

   Sub := -- Check counting of same type with subscription
     (
      Just (0)
      &
        Count (First => 0)
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Nosub := -- Check counting of same type plus type-preserving without subscription
     -
     (
      Just (0)
      &
        Count (First => 0)
      &
        No_Op
     );

   Sub := -- Check counting of same type plus type-preserving plus subscription
     (
      Just (0)
      &
        Count (First => 0)
      &
        No_Op
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Nosub := -- Check counting of different types without subscription
     -
     (
      Just ("hello, world!")
      &
        Count (First => 0)
     );

   Nosub := -- Check mixed counting of different types without subscription
     -
     (
      Strings.From (Strings.Arrays.Build ("hello, world!", "so long, john..."))
      &
        Count (First => 0)
      &
        Count (First => 0)
     );

   Sub := -- Check counting of different types with subscription
     (
      Just ("hello, world!")
      &
        Count (First => 0)
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Sub := -- Check mixed counting of different types with subscription
     (
      Strings.From (Strings.Arrays.Build ("hello, world!", "so long, john..."))
      &
        Count (First => 0)
      &
        Count (First => 0)
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Sub := -- Check mixed counting of indefinite array types with subscription
     (
      Strings.From (Strings.Arrays.Build ("hello, world!", "so long, john..."))
      &
        Count (First => 0)
      &
        Subscribe (Debug.Put_Line'Access)
     );

end Rx.Examples.Minimal;
