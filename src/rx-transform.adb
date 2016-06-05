with Rx.Debug;

package body Rx.Transform is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Operator;
      Consumer : in out Into.Consumers.Observer'Class)
   is
      Parent : From.Producers.Observable'Class := Producer.Get_Parent;
   begin
      Producer.Child := Into.Consumers.To_Holder (Consumer);
      Parent.Subscribe (Producer);
   end Subscribe;

   overriding procedure OnNext (This : in out Operator; V : From.T)
   is
   begin
      Operator'Class (This).On_Next (This.Child.Ref, V);
   end OnNext;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Producers.Observable'Class;
                 R : Operator'Class)
                 return Into.Producers.Observable'Class
   is
      use Rx.Debug;
      Actual : Operator'Class := R;
   begin
      Debug.Log ("chaining " & Image (L'Tag) & " --> " & Image (R'Tag));
      Actual.Set_Parent (L);
      return Actual;
   end "&";

end Rx.Transform;
