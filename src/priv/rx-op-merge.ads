with Rx.Preservers;

generic
   with package Preserver is new Rx.Preservers (<>);
package Rx.Op.Merge is

   function Create (Merge_With : Preserver.Observable'Class) return Preserver.Operator'Class;

end Rx.Op.Merge;
