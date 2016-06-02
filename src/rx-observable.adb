with Rx.Just;

package body Rx.Observable is

   function Wrapped (O : Base.Observable'Class) return Observable'Class
   is (Observable'(Base.Observable with Untyped => Producers.To_Holder (O)));

   ----------
   -- Just --
   ----------

   function Just (V : T) return Observable'Class is
   begin
      return Wrapped (Rx.Just.Create (Values.Wrap (V)));
   end Just;


   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : Consumers.Observer'Class) is
   begin
      if Producer.Untyped.Is_Empty then
         raise Program_Error with "Subscribing to empty observable";
      else
         Producer.Untyped.Ref.Subscribe (Consumer);
      end if;
   end Subscribe;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (O        : Observable;
                        On_Next  : Typed_Actions.Typed_Proc1 := null) is
   begin
      Base.Observable (O).Subscribe (Proc (On_Next));
   end Subscribe;

end Rx.Observable;
