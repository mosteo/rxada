with Rx.Collections;
with Rx.Observables;
with Rx.Transform;
with Rx.Typed;

private with Rx.Op.Buffer;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Observable_Collections is

   package Collections is new Rx.Collections (Typed);
   package Observables is new Rx.Observables (Collections.Typed_Lists);
   package Transform_Lists is new Rx.Transform (Typed, Collections.Typed_Lists);


   function Buffer (Every : Positive; Skip : Natural := 0) return Transform_Lists.Operator'Class;

   ------------------
   --  Metachains  --
   ------------------

--     function "&" (Producer : Typed.Observable;
--                   Consumer : Metaobservables.Operator'Class) return Metaobservables.Intoo.Observable
--                   renames Metaobservables.Will_Observe;


   function "&" (Producer : Typed.Observable;
                 Consumer : Transform_Lists.Operator'Class) return Transform_Lists.Intoo.Observable
     renames Transform_Lists.Will_Observe;

private

   subtype T is Typed.T;

   procedure Append (L : in out Collections.List; V : T);

   package RxBuffer is new Rx.Op.Buffer (Transform_Lists,
                                         Collections.Lists.Empty_List);

   function Buffer (Every : Positive; Skip : Natural := 0) return Transform_Lists.Operator'Class
                    renames RxBuffer.Create;

end Rx.Observable_Collections;
