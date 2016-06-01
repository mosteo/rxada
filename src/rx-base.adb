with Rx.Map;
with Rx.Subscribe;

package body Rx.Base is

   ---------
   -- Map --
   ---------

   function Map
     (O : Observable;
      F : Rx.Actions.Func1'Class)
      return Observable'Class
   is
   begin
      return Rx.Map.Operator'(null record);
   end Map;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (O : Observable;
      P : Rx.Actions.Proc1'Class := Actions.No_Op)
   is
   begin
      I.Observable'Class (O).Subscribe (Internal.Subscribe.As (P));
   end Subscribe;

end Rx.Base;
