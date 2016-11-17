with Rx.Subscribe;

package body Rx.Debug.Observers is

   package RxSubscribe is new Rx.Subscribe (Typed);

   use Typed.Type_Traits;

   type Checker is new RxSubscribe.Subscribe with record
      Counter   : Natural := 0;
      Last_Seen : Typed.D;

      Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.D := +Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.D := +Default_T;
   end record;

   overriding procedure Do_On_Next      (This : in out Checker; V : Typed.T);
   overriding procedure Do_On_Completed (This : in out Checker);

   ----------------
   -- Do_On_Next --
   ----------------

   overriding procedure Do_On_Next      (This : in out Checker; V : Typed.T) is
   begin
      if This.Do_First and then This.Counter = 0 and then V /= +This.Ok_First then
         raise Constraint_Error with
           "Failed first, got [" & Image (V) & "] instead of [" & Image (+This.Ok_First) & "]";
      end if;

      This.Last_Seen := +V;
      This.Counter   := This.Counter + 1;
   end Do_On_Next;

   ---------------------
   -- Do_On_Completed --
   ---------------------

   overriding procedure Do_On_Completed (This : in out Checker) is
   begin
      if This.Do_Count and then This.Counter /= This.Ok_Count then
         raise Constraint_Error with
           "Failed count, got [" & This.Counter'Img & "] instead of [" & This.Ok_Count'Img & "]";
      end if;

      if This.Do_Last and then +This.Last_Seen /= +This.Ok_Last then
         raise Constraint_Error with
           "Failed last, got [" & Image (+This.Last_Seen) & "] instead of [" & Image (+This.Ok_Last) & "]";
      end if;
   end Do_On_Completed;

   -----------------
   -- Checker --
   -----------------

   function Subscribe_Checker
     (Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.T := Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.T := Default_T)
      return Typed.Contracts.Sink'Class
   is
   begin
      return Checker'(RxSubscribe.Subscribe with
                          Do_Count => Do_Count,
                          Ok_Count => Ok_Count,
                          Do_First => Do_First,
                          Ok_First => +Ok_First,
                          Do_Last  => Do_Last,
                          Ok_Last  => +Ok_Last,
                          others   => <>);
   end Subscribe_Checker;

end Rx.Debug.Observers;
