with Rx.Base;
with Rx.Interfaces;
with Rx.Typed_Values;

generic
   type T (<>) is private;
   with function Image (V : T) return String is <>;
package Rx.Observable is

   package I renames Rx.Interfaces;

   package Values is new Typed_Values (T);

   type Observable is new Base.Observable with null record;

   function Just (V : T) return Observable'Class;

private

   overriding
   procedure Subscribe (Producer : Observable;
                        Consumer : I.Observer'Class) is null;

   overriding
   function Just (V : I.Value'Class) return Observable;

end Rx.Observable;
