with Rx.Map;

package body Rx.Operators is

   ---------
   -- Map --
   ---------

   package RxMap is new Rx.Map (Typed);

   function Map (F : Typed.Func1) return Typed.Operator'Class is
   begin
      return RxMap.Create (F);
   end Map;

end Rx.Operators;
