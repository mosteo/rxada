with Rx.Debug;

package body Rx.Subscribe is

   package Defaults renames Typed.Defaults;

   ------------
   -- Create --
   ------------

   function Create (Using : Typed.Observer'Class) return Typed.Sink is
   begin
      return S : Subscribe do
         S.Observer.Hold (Using);
      end return;
   end Create;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Subscribe) is
      use Rx.Actions;
   begin
      if This.Completed then
         raise Program_Error with "Doubly completed";
      elsif
        This.Errored then
         raise Program_Error with "Completed after error";
      end if;

      if This.Is_Subscribed then
         if This.Func_On_Completed /= null then
            This.Func_On_Completed.all;
         elsif This.Observer.Is_Valid then
            This.Observer.Ref.On_Completed;
         end if;
      end if;

      This.Completed := True;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Subscribe; Error : Errors.Occurrence) is
      use Rx.Actions;
   begin
      if This.Errored then
         raise Program_Error with "Doubly errored";
      elsif This.Completed then
         raise Program_Error with "Errored after completed";
      else
         This.Errored := True;
      end if;

      if This.Is_Subscribed then
         if This.Func_On_Error /= null then
            This.Func_On_Error (Error);
         elsif This.Observer.Is_Valid then
            This.Observer.Ref.On_Error (Error);
         end if;
      else
         Debug.Log ("RxAda Subscribe saw an error post-unsubscription:", Debug.Warn);
         Defaults.Default_On_Error (Error);
      end if;
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Subscribe; V : Typed.Type_Traits.T) is
      use Typed.Actions;
   begin
      if This.Is_Subscribed then
         if This.Func_On_Next /= null then
            This.Func_On_Next (V);
         elsif This.Observer.Is_Valid then
            This.Observer.Ref.On_Next (V);
         end if;
      else
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Completed : Rx.Actions.Proc0      := null;
                    On_Error     : Proc_Error            := Typed.Defaults.Default_On_Error'Access)
                    return Typed.Contracts.Sink'Class is
   begin
      return S : Subscribe do
         S.Func_On_Next	     := On_Next;
         S.Func_On_Completed := On_Completed;
         S.Func_On_Error     := On_Error;
      end return;
   end Create;

end Rx.Subscribe;
