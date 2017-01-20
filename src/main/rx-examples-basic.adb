with Rx.Debug; use Rx.Debug;
with Rx.Std;   use Rx.Std;

procedure Rx.Examples.Basic is
   use Integers;
   use Strings;
   use String_To_Integer;
   use Integer_To_String;
   use Numeric.Integers;


   procedure Custom_Src_1 (Observer : in out Integers.Typed.Observer) is
   begin
      Observer.On_Next (7);
      Observer.On_Next (8);
      Observer.On_Next (9);
      Observer.On_Complete ;
   end Custom_Src_1;

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

   Debug.Put_Line ("Custom observable example");
   Sub :=
     Create (Custom_Src_1'Access) &
     Subscribe (Put_Line'Access);

   Debug.Put_Line ("Custom observable with closure example");
   --  I think this must go down in flames if the scope is outlived by the chain, with some latency inducing operator
   --  Since there's no accessibility check kicking in, I guess this is a bug in Gnat and I should use a
   --  named access type in Rx.Src.Create.
   --  TODO: keep an eye on it (Issue #18)
   declare
      procedure Custom_Src_2 (Observer : in out Integers.Typed.Observer) is
      begin
         Observer.On_Next (4);
         Observer.On_Next (5);
         Observer.On_Next (6);
         Observer.On_Complete ;
      end Custom_Src_2;
   begin
      Sub :=
        Create (Custom_Src_2'Access) &
        Subscribe (Put_Line'Access);
   end;

exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
