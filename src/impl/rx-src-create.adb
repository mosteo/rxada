with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Src.Create is

   ------------
   -- Create --
   ------------

   function Create (State : Initial_State) return Observable'Class is
   begin
      return Observable'(S => State);
   end Create;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Observable;
      Consumer : in out Typed.Subscriber)
   is
   begin
      On_Subscribe (Producer.S, Consumer);
      if Completes then
         Consumer.On_Completed;
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("At Create.Subscribe: caught No_Longer_Subscribed");
      when E : others =>
         Typed.Default_Error_Handler (Consumer, E);
   end Subscribe;

end Rx.Src.Create;
