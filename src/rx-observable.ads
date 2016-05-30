with Rx.Interfaces;

generic
   type T (<>) is private;
package Rx.Observable is

   package I renames Rx.Interfaces;

   type Observable is new I.Observable with private;

   function Just (V : T) return Observable;

   function Subscribe (O : Observable) return I.Observer'Class;

   generic
      type R (<>) is private;
   package To is

      type Mapper is access function (V : T) return R;

      function Map (M : Mapper) return I.Operator'Class;

   end To;

private

   type Observable is new I.Observable with null record;

   overriding
   procedure Subscribe   (O : Observable;
                          S : I.Observer'Class) is null;

end Rx.Observable;
