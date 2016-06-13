package body Rx.Links is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Link;
      Consumer : in out Into.Consumers.Observer'Class)
   is
      use type Into.Consumers.Holder;
      Parent : From.Producers.Observable'Class := Producer.Get_Parent;
   begin
      Producer.Child := +Consumer;
      Parent.Subscribe (Producer);
   end Subscribe;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Link; V : From.Type_Traits.T)
   is
   begin
      Link'Class (This).On_Next (This.Child.Ref, V);
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This : in out Link) is
   begin
      Link'Class (This).On_Completed (This.Child.Ref);
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Link; Error : in out Errors.Occurrence) is
   begin
      This.Child.Ref.On_Error (Error); -- Pass it down
   end On_Error;

   ---------
   -- "&" --
   ---------

   function "&" (L : From.Producers.Observable'Class;
                 R : Link'Class)
                 return Into.Producers.Observable'Class
   is
      Actual : Link'Class := R;
   begin
      Actual.Set_Parent (L);
      return Actual;
   end "&";

end Rx.Links;
