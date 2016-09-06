with Rx.Debug;
with Rx.Subscriptions;

package body Rx.Src.Stateless is

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
      Consumer : in out Typed.Consumers.Observer'Class)
   is
   begin
      On_Subscribe (Producer.S, Consumer);
      if Completes then
         Consumer.On_Completed;
      end if;
   exception
      when Subscriptions.No_Longer_Subscribed =>
         Debug.Log ("At Stateless.Subscribe: caught No_Longer_Subscribed");
      when E : others =>
         Consumer.Default_Error_Handler (E);
   end Subscribe;

end Rx.Src.Stateless;
