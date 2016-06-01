private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Rx.Producers.Composite is

   pragma Preelaborate;

   type Observable is new Producers.Observable with private;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Observer'Class);

private

   package Observer_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Observer'Class, Subscribers."=");
   type Obs_List is new Observer_Lists.List with null record;

   type Observable is new Producers.Observable with record
      Observers : Obs_List;
   end record;

end Rx.Producers.Composite;
