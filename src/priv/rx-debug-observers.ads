with Rx.Impl.Typed;

generic
   with package Typed is new Rx.Impl.Typed (<>);
   Default_T : Typed.T;
   with function Image (V : Typed.T) return String is <>;
package Rx.Debug.Observers is

   --  Observer classes with a precise behaviour for testing

   function Subscribe_Checker
     (Name     : String;
      Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.T := Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.T := Default_T;
      Do_Watch : Boolean := True;
      Period   : Duration:= 1.0) return Typed.Sink
     with Pre => Do_Count or Do_First or Do_Last;

   function Subscribe
     (Name     : String;
      Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.T := Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.T := Default_T;
      Do_Watch : Boolean := True;
      Period   : Duration:= 1.0) return Typed.Sink
      renames Subscribe_Checker;

   function Subscribe_Count_Printer return Typed.Sink;
   -- Counts items seen and prints them, counting in both thread safe/unsafe way

end Rx.Debug.Observers;
