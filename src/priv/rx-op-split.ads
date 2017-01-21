with Rx.Impl.Transformers;

generic
   with package Transform is new Rx.Impl.Transformers (<>);
   with procedure Iterate (V : Transform.From.T;
                           For_Each : access procedure (V : Transform.Into.T)) is <>;
package Rx.Op.Split is

   function Create return Transform.Operator'Class;

end Rx.Op.Split;
