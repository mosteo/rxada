with GNAT.OS_Lib;

with Rx.Debug.Heavy;
with Rx.Errors;
with Rx.Tools.Shared_Data;
with Rx.Subscribe;

package body Rx.Debug.Observers is

   use Typed.Conversions;

   package RxSubscribe is new Rx.Subscribe (Typed);

   task type Watchdog (Period_Millis : Integer;
                       Name : not null access String) is
      entry Finished;
   end Watchdog;

   type Watchdog_Access is access all Watchdog;

   task body Watchdog is
      Name_Copy : constant String := Name.all;
   begin
      if Name_Copy'Length < 1 then
         Log ("Empty name in watchdog, exiting", Error);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      select
         accept Finished;
      or
         delay Duration (Period_Millis) / 1000.0;
         Log (Name_Copy & ": watchdog triggered after" & Period_Millis'Img & " ms, exiting", Error);
         GNAT.OS_Lib.OS_Exit (1);

         select
            accept Finished;
         or
            terminate;
         end select;
      end select;
   end Watchdog;

   subtype Checker_Parent is Typed.Defaults.Observer;
   type Checker (Name_Len : Natural) is new Checker_Parent with record
      Name      : String (1 .. Name_Len);

      Counter   : Natural := 0;
      Last_Seen : Typed.D;

      Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.D := + Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.D := + Default_T;

      Do_Watch : Boolean := True;
      Watcher  : Watchdog_Access;
   end record;

   overriding procedure On_Next      (This : in out Checker; V : Typed.T);
   overriding procedure On_Complete  (This : in out Checker);
   overriding procedure On_Error     (This : in out Checker; Error : Errors.Occurrence);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next      (This : in out Checker; V : Typed.T) is
   begin
      Log ("debug.observer on_next enter", Note);
      if This.Do_First and then This.Counter = 0 and then V /= + This.Ok_First then
         Debug.Log ("Failed first, got [" & Image (V) & "] instead of [" & Image (+This.Ok_First) & "]", Debug.Error);
         Heavy.Current_Backtrace (Bailout => True);
      end if;

      This.Last_Seen := +V;
      This.Counter   := This.Counter + 1;
      Log ("debug.observer on_next exit", Note);
   exception
      when others =>
         Log ("debug.observer on_next exit with exception", Note);
         raise;
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Checker) is
   begin
      Log ("debug.observer on_completed enter", Note);
      if This.Watcher /= null then
         This.Watcher.Finished;
      end if;

      if This.Do_Count and then This.Counter /= This.Ok_Count then
         Debug.Log ("Failed count, got [" & This.Counter'Img & "] instead of [" & This.Ok_Count'Img & "]", Debug.Error);
         Heavy.Current_Backtrace (Bailout => True);
      end if;

      if This.Do_Last and then +This.Last_Seen /= +This.Ok_Last then
         Debug.Log ("Failed last, got [" & Image (+This.Last_Seen) & "] instead of [" & Image (+This.Ok_Last) & "]", Debug.Error);
         Heavy.Current_Backtrace (Bailout => True);
      end if;

      Log ("OK " &
           (if This.Do_First then Trim (Image (+This.Ok_First)) & " " else "_ ")
           & (if This.Do_Last then Trim (Image (+This.Ok_Last)) & " " else "_ ")
           & (if This.Do_Count then Trim (This.Counter'Img) else "_")
           & (if This.Name /= "" then " (" & This.Name & ")" else ""),
           Info);
      Log ("debug.observer on_completed exit", Note);
   exception
      when others =>
         Log ("debug.observer on_completed exit with exception", Note);
         raise;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error     (This : in out Checker; Error : Errors.Occurrence) is
   begin
      if This.Watcher /= null then
         This.Watcher.Finished;
      end if;
      Checker_Parent (This).On_Error (Error);
   end On_Error;

   -----------------
   -- Checker --
   -----------------

   function Subscribe_Checker
     (Name     : String;
      Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.T := Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.T := Default_T;
      Do_Watch : Boolean := True;
      Period   : Duration:= 1.0)
      return Typed.Contracts.Sink'Class
   is
      pragma Warnings (Off); -- anon allocator
      Dog : constant Watchdog_Access :=
              (if Do_Watch
               then new Watchdog (Integer (Period * 1000), new String'(Name))
               else null);
      pragma Warnings (On);
   begin
      return RxSubscribe.Create (Checker'(Checker_Parent with
                                 Name_Len => Name'Length,
                                 Name     => Name,
                                 Do_Count => Do_Count,
                                 Ok_Count => Ok_Count,
                                 Do_First => Do_First,
                                 Ok_First => +Ok_First,
                                 Do_Last  => Do_Last,
                                 Ok_Last  => +Ok_Last,
                                 Do_Watch => Do_Watch,
                                 Watcher  => Dog,
                                 others   => <>));
   end Subscribe_Checker;

   ---------------
   --  Counter  --
   ---------------

   type Nat_Ptr is access Natural;

   package Safe_Natural is new Rx.Tools.Shared_Data (Natural, Nat_Ptr);

   type Counter is new Typed.Defaults.Observer with record
      Count      : Natural := 0;
      Safe_Count : Safe_Natural.Proxy := Safe_Natural.Wrap (new Natural'(0));
   end record;

   overriding procedure On_Next      (This : in out Counter; V : Typed.T);
   overriding procedure On_Complete  (This : in out Counter);

   ----------------
   -- Do_On_Next --
   ----------------

   overriding procedure On_Next      (This : in out Counter; V : Typed.T) is
      pragma Unreferenced (V);
      procedure Inc (I : in out Natural) is
      begin
         I := I + 1;
      end Inc;
   begin
      This.Count := This.Count + 1;
      This.Safe_Count.Apply (Inc'Access);
   end On_Next;

   ---------------------
   -- Do_On_Complete  --
   ---------------------

   overriding procedure On_Complete  (This : in out Counter) is
   begin
      if This.Count /= This.Safe_Count.Get then
         Put_Line ("Safe count:  " & Natural'Image (This.Safe_Count.Get));
         Put_Line ("Unsafe count:" & Natural'Image (This.Count));
         Heavy.Current_Backtrace (Bailout => True);
      end if;
   end On_Complete ;

   -----------------------------
   -- Subscribe_Count_Printer --
   -----------------------------

   function Subscribe_Count_Printer return Typed.Sink is
     (RxSubscribe.Create (Counter'(Typed.Defaults.Observer with others => <>)));

end Rx.Debug.Observers;
