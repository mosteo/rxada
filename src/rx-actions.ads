with Rx.Errors;
with Rx.Tools.Holders;

package Rx.Actions with Preelaborate is

   --  Procedures/Actions that do not require a type

   type Inspector is access
     procedure (Event_Kind         : Rx_Event_Kinds;
                Since_Previous     : Duration;
                Since_Subscription : Duration);

   type Proc0      is access procedure;
   type Proc_Error is access procedure (E : Errors.Occurrence);
   type TProc0 is interface;
   procedure Run (Proc : in out TProc0) is abstract;
   function Wrap (Proc : Proc0) return TProc0'Class;

   type Filter0 is access function return Boolean;
   type TFilter0 is interface;
   function Check (Filter : in out TFilter0) return Boolean is abstract;
   function Wrap (Check : Filter0) return TFilter0'Class;

   --  Holders for the tagged variants follow

   package Proc0_Holders is new Rx.Tools.Holders (TProc0'Class);
   type HTProc0 is new Proc0_Holders.Definite with null record;

   package Filter0_Holders is new Rx.Tools.Holders (TFilter0'Class);
   type HTFilter0 is new Filter0_Holders.Definite with null record;

   --  Predefined actions follow

   function Count (Times : Positive) return TFilter0'Class;
   --  At and after the Times-nth call it will return true
   --  E.g. for Times = 3, Check returns False, False, True

   function "not" (Filter : TFilter0'Class) return TFilter0'Class;
   --  Negates the result of some filter

   function Negate (Filter : TFilter0'Class) return TFilter0'Class renames "not";

end Rx.Actions;
