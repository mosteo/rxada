with Rx.Subscribe;

package body Rx.Base is

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (O       : Observable;
      On_Next : Rx.Actions.Proc1'Class := Rx.Actions.No_Op)
   is
      Actual : Observable'Class := O;
   begin
      Actual.Subscribe (Rx.Subscribe.As (On_Next));
   end Subscribe;

end Rx.Base;
