with Rx.Actions;
with Rx.Producers;
with Rx.Traits.Types;

generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Typed is

--     pragma Preelaborate;

   package Actions   is new Rx.Actions   (Type_Traits.T);
   package Producers is new Rx.Producers (Type_Traits.T);
   package Consumers renames Producers.Consumers;

   -- Shortcuts
   subtype T is Type_Traits.T;
   subtype D is Type_Traits.D;
   subtype Observable is Producers.Observable'Class;
   subtype Observer   is Consumers.Observer'Class;

end Rx.Typed;
