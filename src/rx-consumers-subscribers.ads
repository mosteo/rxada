with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
package Rx.Consumers.Subscribers is

   --  Root consumer

   pragma Preelaborate;

   type Subscriber is abstract new Observer with private;

   procedure Set_Parent (This : in out Subscriber; Parent : Producers.Observable'Class);

private

   type Subscriber is abstract new Observer with record
      Parent : Producers.Holder;
   end record;

end Rx.Consumers.Subscribers;
