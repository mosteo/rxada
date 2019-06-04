package body Rx.Schedulers.Pools is

   ------------
   -- Create --
   ------------

   function Create (Size : Positive; Name : String := "") return Pool is
   begin
      if Name = "" then
         return Create (Size, "anon of size" & Size'Img);
      else
         return Unused : Pool (Size, new String'(Name));
      end if;
   end Create;

end Rx.Schedulers.Pools;
