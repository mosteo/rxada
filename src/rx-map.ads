with Rx.Operator;
with Rx.Consumer.Vectors;

generic
   with package Input is new Rx.Operator (<>);
   type Result (<>) is private;
   with function Transform (I : Input.T) return Result;
package Rx.Map is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (Result);

private

   package SM is new Output.Binding.Downstream.Vectors;

   type Operator is new Output.Observable and Input.Observer with record
      Subscribers : SM.SubscriptorManager;
   end record;

   overriding
   procedure OnNext (This : in out Operator; V : Input.T);

   overriding
   procedure Subscribe (O : in out Operator;
                        S : access Output.Observer'Class);

end Rx.Map;
