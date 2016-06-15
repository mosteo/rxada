with Ada.Unchecked_Deallocation;

with Gnat.IO; use Gnat.IO;

with Rx.Debug;

package body Rx.Holders is

   Debug : constant Boolean := True;

   protected Counter is
      procedure Add (I : Integer; Msg : String);
   private
      Count : Integer := 0;
   end Counter;

   protected body Counter is
      procedure Add (I : Integer; Msg : String) is
      begin
         Count := Count + I;
         Put_Line (Id & ": " & Msg & ": instances:" & Count'Img);
      end Add;
   end Counter;

   function "+" (I : Indef) return Definite is
   begin
      if Debug then
         Counter.Add (1, "alloc (+)");
      end if;
      return (Controlled with Actual => new Indef'(I));
   end "+";

   ----------
   -- Hold --
   ----------

   procedure Hold (D : in out Definite; I : Indef) is
   begin
      if D.Actual /= null then
         D.Finalize;
      end if;
      D.Actual := new Indef'(I);
      if Debug then
         Counter.Add (1, "alloc (hold) " & Image (I));
      end if;
   end Hold;

----------------
-- Initialize --
----------------

   overriding procedure Initialize (D : in out Definite) is
   begin
      if D.Actual /= null then
         --           Put_Line ("initialize");
         raise Program_Error;
      end if;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (D : in out Definite) is
   begin
      if D.Actual /= null then
         if Debug then
            Counter.Add (1, "alloc (adjust) " & Image (D.Actual.all));
         end if;
         D.Actual := new Indef'(D.Actual.all);
      end if;
   exception
      when E : others =>
         Put_Line (Id & ": alloc exception (adjust)");
         Rx.Debug.Print (E);
         raise;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (D : in out Definite) is
      procedure Free is new Ada.Unchecked_Deallocation (Indef, Indef_Access);
   begin
      if D.Actual /= null then
         if Debug then
            Counter.Add (-1, "free (finalize) " & Image (D.Actual.all));
         end if;
         Free (D.Actual);
      end if;
   exception
      when E : others =>
         Put_Line (Id & ": alloc exception (finalize)");
         Rx.Debug.Print (E);
         raise;
   end Finalize;

end Rx.Holders;
