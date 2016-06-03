with Rx.Debug;

procedure Rx.Examples.Basic is
   use Strings;
begin
   -- DOT NOTATION
   Strings
     .Just ("Hello, world!")
     --     .Map (StrToInt.Func (Length'Access))
     .Subscribe (Debug.Put_Line'Access);

   -- CONCATENATION
   Strings.Chain := -- Must match the final type
     Just ("Jelly world!") &
     Subscribe (Debug.Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
