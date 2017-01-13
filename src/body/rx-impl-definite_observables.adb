package body Rx.Impl.Definite_Observables is

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Observable;
      Consumer : in out Contracts.Observer'Class)
   is
   begin
      Producer.Ref.Subscribe (Consumer);
   end Subscribe;

   ----------
   -- From --
   ----------

   function From (Indef : Contracts.Observable'Class) return Observable is
   begin
      return Hold (Indef);
   end From;

   ----------
   -- From --
   ----------

   procedure From (This : in out Observable; Indef : Contracts.Observable'Class) is
   begin
      This.Hold (Indef);
   end From;

end Rx.Impl.Definite_Observables;
