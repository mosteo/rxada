with Rx.Debug;

procedure Rx.Examples.Basic is
   use Integers.Observables;
   use Strings.Observables;
   use StrToInt;
   use IntToStr;
begin
   Debug.Put_Line("Just test");
   Chain :=
     Just ("Hello, world!") &
     Map (Length'Access) &
     Map (Image'Access) &
     Map (Length'Access) &
     Subscribe (Debug.Put_Line'Access);
   --  This should print " 3":
   -- "Hello, world!" --> 13 --> " 13" --> 3 --> Integer'Image (3)

   Debug.Put_Line("From_Array test");
   Chain :=
     Integers.Observables.From ((5, 4, 3, 2, 1)) &
     Subscribe (Debug.Put_Line'Access);

   Debug.Put_Line ("Count test");
   Chain :=
     StrToInt."&" (Just ("xxx"), Strings.Instance.Count (0)) &
     Subscribe (Debug.Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
