with Rx.Typed;

generic
   with package Typed is new Rx.Typed (<>);
   Default_T : Typed.T;
   with function Image (V : Typed.T) return String is <>;
package Rx.Debug.Observers is

   --  Observer classes with a precise behaviour for testing

   function Subscribe_Checker
     (Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.T := Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.T := Default_T) return Typed.Contracts.Sink'Class
     with Pre => Do_Count or Do_First or Do_Last;

   function Subscribe
     (Do_Count : Boolean := False;
      Ok_Count : Natural := 0;
      Do_First : Boolean := False;
      Ok_First : Typed.T := Default_T;
      Do_Last  : Boolean := False;
      Ok_Last  : Typed.T := Default_T) return Typed.Contracts.Sink'Class
      renames Subscribe_Checker;

end Rx.Debug.Observers;
