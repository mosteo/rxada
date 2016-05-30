package body Rx.Interfaces is

   type Dummy_Observer is new Observer with null record;

   overriding
   procedure OnNext (This : Dummy_Observer; V : Value'Class) is null;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (Producer : Observable'Class) is
   begin
      Producer.Subscribe (Dummy_Observer'(null record));
   end Subscribe;

   function "&"
     (Producer : Observable'Class;
      Consumer : Operator'Class)
      return Observable'Class
   is
   begin
      Producer.Subscribe (Consumer);
      return Consumer;
   end "&";

end Rx.Interfaces;
