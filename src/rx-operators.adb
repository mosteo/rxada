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

   ---------
   -- "&" --
   ---------

   function "&"
     (L : From.Typed.Producers.Observable'Class;
      R : Into.Typed.Consumers.Observer'Class)
      return Typed.Operator'Class
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, """&"" unimplemented");
      raise Program_Error with "Unimplemented function ""&""";
      return "&" (L => L, R => R);
   end "&";

end Rx.Operators;
