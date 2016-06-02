with Rx.Debug;

procedure Rx.Examples.Basic is
begin
   Strings
     .Just ("Hello, world!")
     --     .Map (StrToInt.Func (Length'Access))
     .Subscribe (Debug.Put_Line'Access);
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
