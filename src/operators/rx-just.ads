with Rx.Holder;
with Rx.Operator;

with Ada.Finalization;

generic
   type T (<>) is private;
   V : T;
package Rx.Just is

   pragma Elaborate_Body;

   package Output is new Rx.Operator (T);

private

   package Holder is new Rx.Holder (T);

   type Control (Parent : access Output.Observable'Class) is new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Finalize (X : in out Control);

   type Observable is new Output.Observable with record
      V : Holder.TH;
      C : Control (Observable'Access);
   end record;

   overriding procedure Subscribe (O : in out Observable;
                        S : access Output.Observer'Class);

end Rx.Just;
