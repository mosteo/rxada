with Rx.Debug;

procedure Rx.Examples.Basic is
begin
   Strings
     .Just ("XXX")
     .Map (StrToInt.Func (Length'Access))
     .Subscribe;
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
