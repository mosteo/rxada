with Rx.Holder;
with Rx.Operator;

generic
   type T (<>) is private;
   V : T;
package Rx.Just is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (T);

private

   package Holder is new Rx.Holder (T);

   type Observable is new Output.Observable with record
      V : Holder.TH;
   end record;

   overriding procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class);

end Rx.Just;
