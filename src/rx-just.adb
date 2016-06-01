with Rx.I;

package body Rx.Just is

   ----------
   -- Just --
   ----------

   function Create
     (V : I.V.Value'Class)
      return Base.Observable'Class
   is
   begin
      return Observable'(Base.Observable with Value => Holder.Hold (V));
   end Create;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe
     (Producer : in out Observable;
      Consumer : I.S.Observer'Class)
   is
   begin
      Consumer.OnNext (Producer.Value.Constant_Reference);
      Consumer.OnCompleted;
   end Subscribe;

end Rx.Just;
