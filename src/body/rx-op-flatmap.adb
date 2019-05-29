package body Rx.Op.Flatmap is

   ------------
   -- Create --
   ------------

   function Create (Func   : Transformer.Actions.Flattener1)
                    return Transformer.Operator'Class is
   begin
      raise Unimplemented;

      return Create (Func);
   end Create;

end Rx.Op.Flatmap;
