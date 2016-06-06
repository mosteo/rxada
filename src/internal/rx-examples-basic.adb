with Rx.Debug;

procedure Rx.Examples.Basic is
   use Integers;
   use Strings;
   use StrtoInt;
   use IntToStr;
begin
   Chain :=
     Strings.Sources.Just ("Hello, world!") &
     Map (Length'Access) &
     Map (Image'Access) &
     Map (Length'Access) &
     Integers.Sources.Subscribe (Debug.Put_Line'Access);
   --  This should print " 3":
   -- "Hello, world!" --> 13 --> " 13" --> 3 --> Integer'Image (3)
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
