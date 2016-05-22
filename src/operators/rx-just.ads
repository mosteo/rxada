generic
   type T (<>) is private;
   V : T;
package Rx.Just is

   pragma Elaborate_Body;

   package Output is new Rx.Base (T);

private

   type Observable is new Output.Observable with record
      V : Output.TH;
   end record;

   overriding procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class);

   Instance : aliased Observable := (V => Output.Hold (Just.V));

end Rx.Just;
