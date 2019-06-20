with Rx.Impl.Transformers;

generic
   with package Transformer is new Rx.Impl.Transformers (<>);

   --  Following identities allow recursive versions in the preserver case,
   --  by bridging the From-Into artificial division in Transformer
   with function Identity (This : Transformer.From.Observer'Class)
                           return Transformer.Into.Observer'Class;
   with function Identity (This : Transformer.From.T)
                           return Transformer.Into.T;

   --  This special Concatenate version allows supplying an arbitrary
   --  AA-AA-AB-BB-BB chain as Into.Observable argument (the chain subscribed
   --  to every upstream Just (V)).
   with procedure Set_Parent (This   : in out Transformer.Into.Observable'Class;
                              Parent :        Transformer.From.Observable'Class);
package Rx.Op.Flatmap is

   --  Identity only makes sense when Transformer is, in reality, a preserver
   --  This allows, in turn, recursive subscription to the observables generated
   --  by Func.
   --  For the same reason, recursive only works in Preserver cases

   function Create (Func      : Transformer.Actions.Inflater1;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class;

   function Create (Func      : Transformer.Actions.TInflater1'Class;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class;

private

   function Create (Func      : Transformer.Actions.Inflater1;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class is
      (Create (Transformer.Actions.Wrap (Func), Recursive));

end Rx.Op.Flatmap;
