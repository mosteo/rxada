with Rx.Debug;

procedure Rx.Examples.Basic is
   use Integers;
   use Strings;
   use StrtoInt;

begin
   Strings.Chain :=
     Just ("Hello, world!") &
     Map (Length'Access)   &
     Subscribe (Debug.Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
