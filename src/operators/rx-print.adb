with Gnat.IO;

package body Rx.Print is

   type Op (Func : Func1) is new Operate.Transform.Operator with null record;

   overriding procedure On_Next (This : in out Op; V : Operate.T) is
      use Gnat.IO;
   begin
      if This.Func /= null then
         Put_Line (This.Func (V));
      else
         Put_Line ("print"); -- Mmm
      end if;
   end On_Next;

   ------------
   -- Create --
   ------------

   function Create (Func : Func1 := null) return Operate.Operator is
   begin
      return Op'(Operate.Transform.Operator with Func => Func);
   end Create;

end Rx.Print;
