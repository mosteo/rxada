package Rx.Interfaces is

   pragma Pure;

   type Value is interface;
   function Image (V : Value) return String is abstract;


   type Observer is interface;
   procedure OnNext      (This : Observer; V : Value'Class) is abstract;
   procedure OnCompleted (This : Observer) is null;


   type Observable is interface;
   procedure Subscribe (Producer : Observable;
                        Consumer : Observer'Class) is abstract;

   type Operator is abstract new Observable and Observer with null record;

end Rx.Interfaces;
