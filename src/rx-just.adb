package body Rx.Just is

   ----------
   -- Just --
   ----------

   function Create
     (V : I.Value'Class)
      return Observable
   is
   begin
      return Observable'(Base.Observable with Value => Holder.Hold (V));
   end Create;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe
     (Producer : Observable;
      Consumer : I.Observer'Class)
   is
   begin
      Consumer.OnNext (Producer.Value.Constant_Reference);
      Consumer.OnCompleted;
   end Subscribe;

end Rx.Just;
