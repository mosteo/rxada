with Rx.Actions;
with Rx.Producers;
with Rx.Traits.Types;

generic
   with package Type_Traits is new Rx.Traits.Types (<>);
package Rx.Typed is

   package Actions   is new Rx.Actions   (Type_Traits.T);
   package Producers is new Rx.Producers (Type_Traits.T);
   package Consumers renames Producers.Consumers;

   -- Middle-chain operator that does not transform types but only does something with values.
   -- E.g., Observe_On
   type Mutator is abstract new Producers.Subscriptor and Producers.Observable with null record;

end Rx.Typed;
