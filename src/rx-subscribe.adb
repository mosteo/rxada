with Rx.Debug;
with Rx.Errors;
with Rx.Subscriptions;

package body Rx.Subscribe is

   type Obs is new Typed.Producers.Subscriptor and Typed.Consumers.Sink with record
      On_Next      : Typed.Actions.Proc1;
      On_Completed : Rx.Actions.Proc0;
      On_Error     : Rx.Actions.Proc_Error;
   end record;

   overriding procedure On_Next      (This : in out Obs; V : Typed.Type_Traits.T);
   overriding procedure On_Completed (This : in out Obs);
   overriding procedure On_Error     (This : in out Obs; Error : in out Errors.Occurrence);

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Obs) is
      use Rx.Actions;
   begin
      if This.On_Completed /= null then
         This.On_Completed.all;
      end if;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Obs; Error : in out Errors.Occurrence) is
      use Rx.Actions;
   begin
      if This.On_Error /= null then
         This.On_Error (Error);
         Error.Set_Handled;
      else
         Debug.Print (Error.Get_Exception.all);
         Error.Reraise;
      end if;
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Obs; V : Typed.Type_Traits.T) is
      use Typed.Actions;
   begin
      if This.On_Next /= null and then This.Subscription.Is_Subscribed then
         This.On_Next (V);
      elsif not This.Subscription.Is_Subscribed then
         raise Subscriptions.No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Completed : Rx.Actions.Proc0      := null;
                    On_Error     : Rx.Actions.Proc_Error := null) return Typed.Producers.Subscriptor'Class is
   begin
      return Obs'(Typed.Producers.Subscriptor with On_Next, On_Completed, On_Error);
   end Create;

end Rx.Subscribe;
