generic
   with package Input is new Rx.Base (<>);
package Rx.Count is

   pragma Elaborate_Body;

   package Output is new Rx.Base (Integer);

private

   type Operator is new Output.Observable and Input.Observer with record
      Count       : Natural := 0;
      Subscribers : Output.Observer_Vectors.Vector;
   end record;

   overriding
   procedure OnNext (This : in out Operator; V : Input.T);

   overriding
   procedure OnCompleted (This : in out Operator);

   overriding
   procedure Subscribe (O : in out Operator;
                        S : access Output.Observer'Class);

   Instance : aliased Operator;

end Rx.Count;
