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

   ------------
   -- Create --
   ------------

   function Create (Secondary : Transformer.Into.Observable'Class;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class;
   --  Secondary is subscribed to Just (From.V) for every On_Next (V)
   --  It must be composed of Operator'Class, even if given as plain Observable
   --    (because that is what concatenation function & returns).
   --  Also, the types From-Into must be respected. That is, it must form a
   --    partial chain (without source nor sink) of AA-AB-BB.

   --  Alternate forms that take an Inflater:

   function Create (Func      : Transformer.Actions.Inflater1;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class;

   function Create (Func      : Transformer.Actions.TInflater1'Class;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class;

   ---------
   -- "&" --
   ---------
   --  As with Map, by having those we can have implicit flatmaps:
   --  Source
   --  & Inflater'Access <-- same as Flat_Map (Inflater'Access)
   --  & Sink

   function "&" (Producer : Transformer.From.Observable'Class;
                 Consumer : Transformer.Actions.Inflater1)
                 return Transformer.Into.Observable'Class;

   function "&" (Producer : Transformer.From.Observable'Class;
                 Consumer : Transformer.Actions.TInflater1'Class)
                    return Transformer.Into.Observable'Class;

private

   function Create (Func      : Transformer.Actions.Inflater1;
                    Recursive : Boolean := False)
                    return Transformer.Operator'Class is
     (Create (Transformer.Actions.Wrap (Func), Recursive));

   function "&" (Producer : Transformer.From.Observable'Class;
                 Consumer : Transformer.Actions.Inflater1)
                 return Transformer.Into.Observable'Class is
     (Transformer.Concatenate (Producer, Create (Consumer)));

   function "&" (Producer : Transformer.From.Observable'Class;
                 Consumer : Transformer.Actions.TInflater1'Class)
       return Transformer.Into.Observable'Class is
     (Transformer.Concatenate (Producer, Create (Consumer)));

end Rx.Op.Flatmap;
