with Rx.Interfaces;

generic
   type T (<>) is private;
package Rx.Observable is

   package I renames Rx.Interfaces;

   type Observable is new I.Observable with private;

   function Just (V : T) return Observable;

   function Subscribe (O : Observable) return I.Observer'Class;

      type Operator is abstract new I.Observable and I.Observer with null record;

   generic
      type R (<>) is private;
   package To is

      type Mapper is access function (V : T) return R;

      function Map (M : Mapper) return Operator'Class;

   end To;

   function "&" (L : I.Observable'Class; R : Operator'Class) return I.Observable'Class;

   procedure Assemble (X : I.Observable'Class);

private

   type Observable is new I.Observable with null record;

   overriding
   procedure Subscribe   (O : in out Observable;
                          S :        I.Observer'Class) is null;

end Rx.Observable;
