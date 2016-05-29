with Ada.Containers.Vectors;

generic
package Rx.Consumer.Vectors is

   type Observer_Access is access all Observer'Class;
   package CV is new Ada.Containers.Vectors (Positive, Observer_Access);

   type SubscriptorManager is new CV.Vector with null record;

end Rx.Consumer.Vectors;
