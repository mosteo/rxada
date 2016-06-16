with Ada.Finalization; use Ada.Finalization;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Unchecked_Deallocation;

procedure Finalize_Leak is
   generic
   package P is

      type One is interface;

      type Int_Access is access Integer;

      type Managed is new Controlled with record
         X : Int_Access;
      end record;
      overriding procedure Adjust (M : in out Managed);
      overriding procedure Finalize (M : in out Managed);
      function Build (I : Integer) return Managed;

      type Two is new One with record
         M : Managed := Build (1);
      end record;

   end P;

   package body P is
      overriding procedure Adjust (M : in out Managed) is
      begin
         if M.X /= null then
            M.X := new Integer'(M.X.all);
         end if;
      end Adjust;

      overriding procedure Finalize (M : in out Managed) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Integer, Int_Access);
      begin
         if M.X /= null then
            Free (M.X);
            Put_Line ("finalize M with free");
         else
            Put_Line ("finalize M");
         end if;
      end Finalize;

      function Build (I : Integer) return Managed
      is (Managed'(Controlled with X => new Integer'(I)));
   end P;

   package PP is new P; use PP;

   function Pass (X : Two'Class) return One'Class is (X);
   function "not" (X : Two'Class) return One'Class is (X);

   A : Two;
begin
   A.M := Build (1);

   for I in 1 .. 666 loop
      Put_Line ("----------------------------");
      declare
         B : One'Class := Pass (A); -- This is properly finalized
      begin
         Put_Line ("......");
      end;
      Put_Line ("......");

      declare
         B : One'Class := not A; -- This is not
      begin
         Put_Line ("---8<---");
      end;
      Put_Line ("--->8---");
   end loop;

   New_Line; Put_Line ("Now A is going to finalize");
end Finalize_Leak;
