with Ada.Calendar.Formatting;

with Ada.Text_IO;

with Rx.Schedulers;

package body Rx.Op.Print is

   use Ada.Calendar;

   function Stamp return String is
      (Formatting.Image (Clock, Include_Time_Fraction => True) & ": ");

   type Op (Func : Operate.Typed.Actions.Func1Str) is new Operate.Operator with record
      With_Timestamp : Boolean := True;
   end record;

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
      use Ada.Text_IO;
      use Operate.Typed.Actions;
   begin
      if This.Func /= null then
         Put_Line ((if This.With_Timestamp then Stamp else "") & This.Func (V));
      else
         Put_Line ((if This.With_Timestamp then Stamp else "") & Rx.Schedulers.Current_Thread_Id); -- Mmm
      end if;
      This.Get_Observer.On_Next (V);
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Func : Operate.Typed.Actions.Func1Str := null; With_Timestamp : Boolean := True) return Operate.Operator'Class is
   begin
      return Op'(Operate.Operator
                             with Func => Func, With_Timestamp => With_Timestamp);
   end Create;

end Rx.Op.Print;
