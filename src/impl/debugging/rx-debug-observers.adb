with Rx.Impl.Shared_Data;
with Rx.Subscribe;

package body Rx.Debug.Observers is

   use Typed.Conversions;

   package RxSubscribe is new Rx.Subscribe (Typed);

   type Checker is new RxSubscribe.Subscribe with record
      Counter   : Natural := 0;
      Last_Seen : Typed.D;

      Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.D := + Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.D := + Default_T;
   end record;

   overriding procedure Do_On_Next      (This : in out Checker; V : Typed.T);
   overriding procedure Do_On_Completed (This : in out Checker);

   ----------------
   -- Do_On_Next --
   ----------------

   overriding procedure Do_On_Next      (This : in out Checker; V : Typed.T) is
   begin
      if This.Do_First and then This.Counter = 0 and then V /= + This.Ok_First then
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

   ---------------
   --  Counter  --
   ---------------

   type Nat_Ptr is access Natural;

   package Safe_Natural is new Rx.Impl.Shared_Data (Natural, Nat_Ptr);

   type Counter is new RxSubscribe.Subscribe with record
      Count      : Natural := 0;
      Safe_Count : Safe_Natural.Proxy := Safe_Natural.Wrap (new Natural'(0));
   end record;

   overriding procedure Do_On_Next      (This : in out Counter; V : Typed.T);
   overriding procedure Do_On_Completed (This : in out Counter);

   ----------------
   -- Do_On_Next --
   ----------------

   overriding procedure Do_On_Next      (This : in out Counter; V : Typed.T) is
      pragma Unreferenced (V);
      procedure Inc (I : in out Natural) is
      begin
         I := I + 1;
      end Inc;
   begin
      This.Count := This.Count + 1;
      This.Safe_Count.Apply (Inc'Access);
   end Do_On_Next;

   ---------------------
   -- Do_On_Completed --
   ---------------------

   overriding procedure Do_On_Completed (This : in out Counter) is
   begin
      if This.Count /= This.Safe_Count.Get then
         Put_Line ("Safe count:  " & Natural'Image (This.Safe_Count.Get));
         Put_Line ("Unsafe count:" & Natural'Image (This.Count));
         raise Constraint_Error
           with "Count was mismatched:" & This.Count'Img & " /=" & Natural'Image (This.Safe_Count.Get);
      end if;
   end Do_On_Completed;

   -----------------------------
   -- Subscribe_Count_Printer --
   -----------------------------

   function Subscribe_Count_Printer return Typed.Sink is (Counter'(RxSubscribe.Subscribe with others => <>));

end Rx.Debug.Observers;
