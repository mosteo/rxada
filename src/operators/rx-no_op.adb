with Ada.Finalization; use Ada.Finalization;

with Gnat.IO; use Gnat.IO;

package body Rx.No_Op is

   type Finish is new Controlled with null record;
   overriding procedure Finalize (F : in out Finish) is
   begin
      Put_Line ("NOP finalize");
   end Finalize;

   type Op is new Operate.Transform.Operator with record
      F : Finish;
   end record;

   overriding procedure On_Next (This : in out Op; V : Operate.T) is null;

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator is
   begin
      return Op'(Operate.Transform.Operator with others => <>);
   end Create;

end Rx.No_Op;
