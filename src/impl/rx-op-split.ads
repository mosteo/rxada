with Rx.Transformers;

generic
   with package Transformer is new Rx.Transformers (<>);
   with procedure Iterate (V : Transformer.From.T;
                           For_Each : access procedure (V : Transformer.Into.T)) is <>;
package Rx.Op.Split is

   function Create return Transformer.Operator'Class;

end Rx.Op.Split;
