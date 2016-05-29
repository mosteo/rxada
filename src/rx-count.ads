with Rx.Consumer.Vectors;
with Rx.Operator;

generic
   with package Input is new Rx.Operator (<>);
package Rx.Count is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (Integer);

private

   package SM is new Output.Binding.Downstream.Vectors; -- Wow...

   type Operator is new Output.Observable and Input.Observer with record
      Count       : Natural := 0;
      Subscribers : SM.SubscriptorManager;
   end record;

   overriding
   procedure OnNext (This : in out Operator; V : Input.T);

   overriding
   procedure OnCompleted (This : in out Operator);

   overriding
   procedure Subscribe (O : in out Operator;
                        S : access Output.Observer'Class);

end Rx.Count;
