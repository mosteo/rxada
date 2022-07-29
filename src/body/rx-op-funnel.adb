with Rx.Debug;
with Rx.Impl.Shared_Observer;
with Rx.Op.Serialize;

package body Rx.Op.Funnel is

   package RxSerialize is new Rx.Op.Serialize  (Preserver);

   package Shared_Observers is new Impl.Shared_Observer (Preserver.Typed);

   type Operator is new Preserver.Operator with null record;

   overriding procedure On_Next     (This : in out Operator; V : Preserver.T);

   overriding procedure Subscribe (This     : in out Operator;
                                   Consumer : in out Preserver.Observer'Class);

   ------------
   -- Create --
   ------------

   function Create return Preserver.Operator'Class is
      use Preserver.Linkers;
   begin
      return
        Preserver.Operator'Class
          (RxSerialize.Create
           & Operator'(Preserver.Operator with null record));
   end Create;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator; V : Preserver.T) is
   begin
      Debug.Trace ("funnel on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This     : in out Operator;
                                   Consumer : in out Preserver.Observer'Class)
   is
      Shared : Shared_Observers.Observer :=
                 Shared_Observers.Create (Consumer,
                                          Checked => False);
   begin
      Preserver.Operator (This).Subscribe (Shared);
   end Subscribe;

end Rx.Op.Funnel;
