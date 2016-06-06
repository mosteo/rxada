with Rx.Actions;
with Rx.Holders;
with Rx.Producers;

generic
   type T (<>) is private; -- User type
package Rx.Typed is

   package Actions   is new Rx.Actions (T);
   package Holders   is new Rx.Holders (T);
   package Producers is new Rx.Producers (T);
   package Consumers renames Producers.Consumers;

end Rx.Typed;
