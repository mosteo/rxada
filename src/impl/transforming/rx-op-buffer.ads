with Rx.Transform;

generic
   with package Transform is new Rx.Transform (<>);
   Empty : Transform.Into.T;
   with procedure Append (Container : in out Transform.Into.T; V : Transform.From.T);
package Rx.Op.Buffer with Preelaborate is

end Rx.Op.Buffer;
