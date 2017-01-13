package body Rx.Traits.Arrays is

   --------------
   -- Builders --
   --------------

   use Typed.Conversions;

   -----------
   -- Build --
   -----------

   function Build
     (V1   : Typed.T;
      More : Typed_Array := Empty_Array)
         return Typed_Array
   is
   begin
      return (Indexes'First => +V1) & More;
   end Build;

   -----------
   -- Build --
   -----------

   function Build
     (V1, V2 : Typed.T;
      More   : Typed_Array := Empty_Array)
         return Typed_Array
   is
   begin
      return (+V1, +V2) & More;
   end Build;

   -----------
   -- Build --
   -----------

   function Build
     (V1, V2, V3 : Typed.T;
      More       : Typed_Array := Empty_Array)
         return Typed_Array
   is
   begin
      return (+V1, +V2, +V3) & More;
   end Build;

   -----------
   -- Build --
   -----------

   function Build
     (V1, V2, V3, V4 : Typed.T;
      More           : Typed_Array := Empty_Array)
         return Typed_Array
   is
   begin
      return (+V1, +V2, +V3, +V4) & More;
   end Build;

   -----------
   -- Build --
   -----------

   function Build
     (V1, V2, V3, V4, V5 : Typed.T;
      More               : Typed_Array := Empty_Array)
         return Typed_Array
   is
   begin
      return (+V1, +V2, +V3, +V4, +V5) & More;
   end Build;

   -----------
   -- Build --
   -----------

   function Build
     (V1, V2, V3, V4, V5, V6 : Typed.T;
      More                   : Typed_Array := Empty_Array)
         return Typed_Array
   is
   begin
      return (+V1, +V2, +V3, +V4, +V5, +V6) & More;
   end Build;

end Rx.Traits.Arrays;
