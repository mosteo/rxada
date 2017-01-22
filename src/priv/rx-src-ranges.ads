with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
   with function Succ (V : Typed.T) return Typed.T is <>;
   with function "<" (L, R : Typed.T) return Boolean is <>;
package Rx.Src.Ranges is

   function From_Count (First : Typed.T; Count : Rx_Natural) return Typed.Observable;

   function From_Slice (First, Last : Typed.T) return Typed.Observable;
   --  Might not emit anything if Last < First

end Rx.Src.Ranges;
