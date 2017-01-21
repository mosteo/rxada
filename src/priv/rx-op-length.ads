with Rx.Impl.Transformers;

generic
   with package Transform is new Rx.Impl.Transformers (<>);
   with function Length (V : Transform.From.T) return Transform.Into.T is <>;
package Rx.Op.Length with Preelaborate is

   function Create return Transform.Operator'Class;
   --  Emits the length of each input item

end Rx.Op.Length;
