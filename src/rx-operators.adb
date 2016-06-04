with Rx.Map;

package body Rx.Operators is

   package RxMap is new Rx.Map (Typed);

   ---------
   -- Map --
   ---------

   function Map (F : Typed.Func1) return Typed.Operator'Class is
   begin
      return RxMap.Create (F);
   end Map;

end Rx.Operators;
