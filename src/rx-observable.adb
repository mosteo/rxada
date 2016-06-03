with Rx.Debug;
with Rx.Just;
with Rx.Subscribe;

package body Rx.Observable is

   function Wrapped (O : Base.Observable'Class) return Observable'Class
   is (Observable'(Base.Observable with Untyped => Producers.To_Definite (O)));

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
                        Consumer : Consumers.Observer'Class)
   is
   begin
      if Producer.Untyped.Is_Empty then
         raise Program_Error with "Subscribing to empty observable";
      else
         Debug.Put_Line ("EMPTY? " & Producer.Untyped.Is_Empty'Img);
         declare
            P : Producers.Observable'Class := Producer.Untyped.Element;
         begin
            P.Subscribe (Consumer);
         end;
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

   ------------
   -- OnNext --
   ------------

   overriding
   procedure OnNext (This : Consumer; V : Rx.Values.Value'Class) is
   begin
      if This.Untyped.Is_Empty then
         raise Program_Error with "OnNext call with empty consumer (shouldn't happen)";
      else
         This.Untyped.Element.OnNext (V);
      end if;
   end OnNext;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (On_Next  : Typed_Actions.Typed_Proc1 := null) return Consumer is
   begin
      return Consumer'
        (Untyped =>
           Consumers.To_Definite
             (Rx.Subscribe.As
                  (Proc (On_Next))));
   end Subscribe;

   ---------
   -- "&" --
   ---------

   function "&" (L : Base.Observable'Class; R : Consumer) return Subscriptions.Subscription is
      Actual : Producers.Observable'Class := L;
   begin
      Actual.Subscribe (R);
--      Actual.Subscribe (R.Untyped.Element); -- Perhaps I could subscribe directly to the wrapped Untyped
      return Chain;
   end "&";

end Rx.Observable;
