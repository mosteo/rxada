with Rx.Impl.Transformers;

generic
   with package Transform is new Rx.Impl.Transformers (<>);
   Empty : Transform.Into.D;
   with procedure Append (Container : in out Transform.Into.D; V : Transform.From.T) is <>;
package Rx.Op.Buffer with Preelaborate is

--  The use of definites in the generic formal presumes some form of definite container that will
--  be the same type as the indefinite


   function Create (Every : Positive; Skip : Natural := 0) return Transform.Operator'Class;
   --  Builds lists of size Every, discarding Skip between each one

end Rx.Op.Buffer;
