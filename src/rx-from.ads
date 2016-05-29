with Rx.Operator;

generic
   type T is private;
   type TA is array (Integer range <>) of T;
   VA : TA;
package Rx.From is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (T);

private

   type Observable (Last : Integer) is new Output.Observable with record
      VA : TA (1 .. Last);
   end record;

   overriding
   procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class);

end Rx.From;
