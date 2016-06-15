with Rx.Consumers;
with Rx.Holders;

with Ada.Tags;

generic
   type T (<>) is private;
package Rx.Producers is

--     pragma Preelaborate;

   package Consumers is new Rx.Consumers (T);

   type Observable is interface;

   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Consumers.Observer'Class) is abstract;

   function Image (Y : Observable'Class) return String;

   package Holders is new Rx.Holders (Observable'Class, "observable'class", Image);
   type Holder is new Holders.Definite with null record;


   type Subscriber is interface;

   procedure Set_Parent (This : in out Subscriber; Parent : Observable'Class) is abstract;
   function  Get_Parent (This :        Subscriber) return Observable'Class is abstract;
   function  Has_Parent (This :        Subscriber) return Boolean is abstract;

   -- Convenience type since we'll need all observers to also be subscribers
   type Subscriptor is abstract new Consumers.Observer and Subscriber with private;

   overriding procedure Set_Parent (This : in out Subscriptor; Parent : Observable'Class);
   overriding function  Get_Parent (This :        Subscriptor) return Observable'Class;
   overriding function  Has_Parent (This :        Subscriptor) return Boolean;

private

   type Subscriptor is abstract new Consumers.Observer and Subscriber with record
      Parent : Holder;
   end record;

   overriding function  Get_Parent (This : Subscriptor) return Observable'Class is (This.Parent.Cref);
   overriding function  Has_Parent (This : Subscriptor) return Boolean is (not This.Parent.Is_Empty);

   function Image (Y : Observable'Class) return String is (Ada.Tags.External_Tag (Y'Tag));

end Rx.Producers;
