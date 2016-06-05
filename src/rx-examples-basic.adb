with Rx.Debug;

procedure Rx.Examples.Basic is
   use Integers;
   use Strings;
   use StrtoInt;
   use IntToStr;
begin
   Integers.Chain :=
     Just ("Hello, world!") &
     Map (Length'Access) &
     Map (Image'Access) &
     Map (Length'Access) &
     Subscribe (Debug.Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
