with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>); -- Items emitted
   with function Succ (V : Typed.T) return Typed.T; -- Next in sequence
package Rx.Src.Sequence is

   pragma Preelaborate;

   function Create (First : Typed.T;
                    Count : Natural)
                    return Typed.Observable;

end Rx.Src.Sequence;
