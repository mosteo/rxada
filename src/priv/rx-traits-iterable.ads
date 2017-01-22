with Rx.Impl.Typed;

pragma Warnings (Off);

generic
   with package Typed is new Rx.Impl.Typed (<>);
   type Container is private;
   with procedure Iterate (C : Container; Proc : access procedure (V : Typed.T)) is <>;
   --  Iterate should call its second parameter procedure for each element in the container
package Rx.Traits.Iterable with Preelaborate is

end Rx.Traits.Iterable;
