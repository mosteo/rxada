generic
   with package Input is new Rx.Base (<>);
   type Result (<>) is private;
   with function Transform (I : Input.T) return Result;
package Rx.Map is

   pragma Elaborate_Body;

   package Output is new Rx.Base (Result);

private

   type Operator is new Output.Observable and Input.Observer with record
      Subscribers : Output.Observer_Vectors.Vector;
   end record;

   overriding
   procedure OnNext (This : in out Operator; V : Input.T);

   overriding
   procedure Subscribe (O : in out Operator;
                        S : access Output.Observer'Class);

   Instance : aliased Operator;

end Rx.Map;
