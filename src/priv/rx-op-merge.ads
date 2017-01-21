with Rx.Impl.Preservers;

generic
   with package Preserver is new Rx.Impl.Preservers (<>);
package Rx.Op.Merge is

   function Create (Merge_With : Preserver.Observable'Class) return Preserver.Operator'Class;

end Rx.Op.Merge;
