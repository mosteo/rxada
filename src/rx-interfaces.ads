package Rx.Interfaces is

   pragma Pure;

--  These should go to different files, but I'm lazy now

   type Value is interface;

   function Image (V : Value) return String is abstract;



   type Observer is interface;

   procedure OnNext      (This : in out Observer; V : Value'Class) is abstract;

   procedure OnCompleted (This : in out Observer) is null;



   type Observable is interface;

   procedure Subscribe   (O : in out Observable;
                          S :        Observer'Class) is abstract;

end Rx.Interfaces;
