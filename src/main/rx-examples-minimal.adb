with Rx.Debug;

--  This procedure tests unambiguous visibility of "&"

procedure Rx.Examples.Minimal is

   use Integers.Observables;
   use Strings.Observables;

--     use Chars.Observables;
--     use Intarrs.Observables;

   use StrToInt;
   use IntToStr;
   use IntCount;
   use StrCount;

begin
   Chain :=
     Just ("Hello, world!")
     & -- Check subscription alone
     Subscribe (Debug.Put_Line'Access);

   Chain :=
     +
     (
      Just ("Hello, world!")
      & -- Check single type-preserving operation alone;
      No_Op
     );

   Chain :=
     +
     (
      Just ("Hello, world!")
      & -- Check multiple type-preserving operation alone;
        No_Op
      &
        No_op
     );

   Chain := -- Check type-preserving op plus subscription
     Just ("Hello, world!")
     &
     No_Op
     &
     Subscribe (Debug.Put_Line'Access);

   Chain :=
     +
     (
      Just ("Hello, world!")
      & -- Check transformation alone;
        Map (Length'Access)
     );

   Chain := -- Check roundtrip
     +
     (
      Just ("Hello, world!")
      &
        Map (Length'Access)
      &
        Map (Image'Access)
     );

   Chain := -- Check roundtrip plus subscription
     (
      Just ("Hello, world!")
      &
        Map (Length'Access)
      &
        Map (Image'Access)
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Chain := -- Check counting of same type without subscription
     +
     (
      Just (0)
      &
        Count (First => 0)
     );

   Chain := -- Check counting of same type with subscription
     (
      Just (0)
      &
        Count (First => 0)
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Chain := -- Check counting of same type plus type-preserving without subscription
     +
     (
      Just (0)
      &
        Count (First => 0)
      &
        No_Op
     );

   Chain := -- Check counting of same type plus type-preserving plus subscription
     (
      Just (0)
      &
        Count (First => 0)
      &
        No_Op
      &
        Subscribe (Debug.Put_Line'Access)
     );

   Chain := -- Check counting of different types without subscription
     +
     (
      Just ("hello, world!")
      &
        Count (First => 0)
     );

   Chain := -- Check counting of different types plus subscription
     (
      From (Strings.Arrays.Build ("hello, world!", "so long, john..."))
      &
        Count (First => 0)
     );

end Rx.Examples.Minimal;
