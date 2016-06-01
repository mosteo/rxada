with Rx.Actions;
with Rx.Debug; use Rx.Debug;
with Rx.Observable;
with Rx.Transform;

procedure Rx.Test is



--   A : Rx.Actions.Func1'Class := StrToInt.Func (Length'Access);

begin

   Strings
     .Just ("XXX")
     .Map (StrToInt.Func (Length'Access))
     .Subscribe;

exception
   when E : others =>
      Rx.Debug.Print (E);
end Rx.Test;
