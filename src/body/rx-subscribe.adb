with Rx.Debug;

package body Rx.Subscribe is

   package Defaults renames Typed.Defaults;

   ------------
   -- Create --
   ------------

   function Create (Using : Typed.Observer'Class) return Typed.Sink is
   begin
      Debug.Trace ("subscribe [create w observer]");
      return S : Subscribe do
         S.Observer.Hold (Using);
      end return;
   end Create;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Subscribe) is
      use Rx.Actions;
   begin
      if This.Completed then
         Debug.Trace ("subscribe [doubly completed]");
         raise Program_Error with "Doubly completed";
      elsif This.Errored then
         Debug.Trace ("subscribe [post-error complete]");
         raise Program_Error with "Completed after error";
      else
         Debug.Trace ("subscribe [on_complete]");
      end if;

      if This.Is_Subscribed then
         if This.Func_On_Complete  /= null then
            This.Func_On_Complete .all;
         elsif This.Observer.Is_Valid then
            This.Observer.Ref.On_Complete ;
         end if;

         Typed.Contracts.Sink (This).On_Complete;
      end if;

      This.Completed := True;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Subscribe; Error : Errors.Occurrence) is
   begin
      if This.Errored then
         Debug.Trace ("subscribe [double on_error]");
         raise Program_Error with "Doubly errored";
      elsif This.Completed then
         Debug.Trace ("subscribe [post-complete on_error]");
         raise Program_Error with "Errored after completed";
      else
         Debug.Trace ("subscribe [on_error]");
         This.Errored := True;
      end if;

      if This.Is_Subscribed then
         if This.Func_On_Error /= null then
            Debug.Trace ("subscribe [func]");
            This.Func_On_Error (Error);
         elsif This.Observer.Is_Valid then
            Debug.Trace ("subscribe [observer]");
            This.Observer.Ref.On_Error (Error);
         else
            Debug.Trace ("subscribe [unhandled]");
            Debug.Report (Error.Get_Exception.all,
                          "Unhandled error in subscriber");
            raise Program_Error with "Unhandled error";
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
            Debug.Trace ("subscribe [on_next func]");
            This.Func_On_Next (V);
         elsif This.Observer.Is_Valid then
            Debug.Trace ("subscribe [on_next observer]");
            This.Observer.Ref.On_Next (V);
         end if;
      else
         Debug.Trace ("subscribe [on_next no_longer_subscribed]");
         raise No_Longer_Subscribed;
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (On_Next      : Typed.Actions.Proc1   := null;
                    On_Complete  : Rx.Actions.Proc0      := null;
                    On_Error     : Proc_Error            := Typed.Defaults.Default_On_Error'Access)
                    return Typed.Contracts.Sink'Class is
   begin
      Debug.Trace ("subscribe [create w funcs]");
      return S : Subscribe do
         S.Func_On_Next	     := On_Next;
         S.Func_On_Complete  := On_Complete ;
         S.Func_On_Error     := On_Error;
      end return;
   end Create;

end Rx.Subscribe;
