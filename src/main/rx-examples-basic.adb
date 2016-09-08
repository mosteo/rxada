with Rx.Debug; use Rx.Debug;

procedure Rx.Examples.Basic is
   use Integers;
   use Strings;
   use StrToInt;
   use IntToStr;
   use IntCount;

begin
   Debug.Put_Line ("Just example");
   Sub :=
     Just ("Hello, world!") &
     Map (Length'Access) &
     Map (Image'Access) &
     Map (Length'Access) &
     Subscribe (Debug.Put_Line'Access);
   --  This should print " 3":
   -- "Hello, world!" --> 13 --> " 13" --> 3 --> Integer'Image (3)

   Debug.Put_Line ("From_Array example");
   Sub :=
     Integers.From ((5, 4, 3, 2, 1)) &
     Subscribe (Debug.Put_Line'Access);

   Debug.Put_Line ("Count example");
   Sub :=
     Integers.From ((0, 1, 2, 3)) &
     Count (First => 0) &
     Subscribe (Debug.Put_Line'Access);

   Debug.Put_Line ("Count reset example");
   declare
      Ob : constant Integers.Observable :=
             Integers.From ((0, 1, 2, 3))
             & Count (First => 0);
   begin
      Sub := Ob & Subscribe (Put_Line'Access); -- Must both output 4
      Sub := Ob & Subscribe (Put_Line'Access); -- Must both output 4
   end;
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
