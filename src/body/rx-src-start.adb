with Rx.Src.Just;

package body Rx.Src.Start is

   package RxJust is new Rx.Src.Just (Typed);

   ------------
   -- Create --
   ------------

   function Create
     (Func : Typed.Actions.TFunc0'Class)
      return Typed.Observable
   is
      Actual : Typed.Actions.TFunc0'Class := Func; -- RW copy
   begin
      return RxJust.Create (Actual.Get);
   end Create;

end Rx.Src.Start;
