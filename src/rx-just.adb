package body Rx.Just is

   ----------
   -- Just --
   ----------

   function Create
     (V : Values.Value'Class)
      return Base.Observable'Class
   is
   begin
      return Observable'(Base.Observable with Value => Values.To_Holder (V));
   end Create;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe
     (Producer : in out Observable;
      Consumer : Consumers.Observer'Class)
   is
   begin
      Consumer.OnNext (Producer.Value.Constant_Reference);
      Consumer.OnCompleted;
   end Subscribe;

end Rx.Just;
