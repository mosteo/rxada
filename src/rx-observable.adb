package body Rx.Observable is

   ----------
   -- Just --
   ----------

   function Just (V : T) return Observable is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Just unimplemented");
      raise Program_Error with "Unimplemented function Just";
      return Just (V);
   end Just;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (O : Observable) return I.Observer'Class is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Subscribe unimplemented");
      raise Program_Error with "Unimplemented function Subscribe";
      return Subscribe (O);
   end Subscribe;

   --------
   -- To --
   --------

   package body To is

      ---------
      -- Map --
      ---------

      function Map (M : Mapper) return I.Operator'Class is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Map unimplemented");
         raise Program_Error with "Unimplemented function Map";
         return Map (M);
      end Map;

   end To;

end Rx.Observable;
