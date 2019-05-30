package body Rx.Op.Sample is

   ------------
   -- Create --
   ------------

   function Create
     (Policy  : Policies;
      Sampler : Samplers.Observable'Class)
      return Operate.Operator'Class
   is
   begin
      raise Unimplemented;
      return Create (Policy, Sampler);
   end Create;

end Rx.Op.Sample;
