with Ada.Finalization; use Ada.Finalization;
with Ada.Text_Io; use Ada.Text_Io;

procedure Finalize_Leak is
   generic
   package P is

      type One is interface;

      type Managed is new Controlled with record
         X : Integer := 1;
      end record;
      overriding procedure Finalize (M : in out Managed);

      type Two is new One with record
         M : Managed;
      end record;

   end P;

   package body P is
      overriding procedure Finalize (M : in out Managed) is
         pragma Unreferenced (M);
      begin
         Put_Line ("finalize M");
         M.X := 0;
      end Finalize;
   end P;

   package PP is new P; use PP;

   function Pass (X : Two'Class) return One'Class is (X);
   function "not" (X : Two'Class) return One'Class is (X);

   A : Two;
begin
   declare
      B : constant One'Class := A;
   begin
      Put_Line ("---8<---");
      Put_Line (Two'Class (B).M.X'Img);
   end;
   Put_Line ("--->8---");

   New_Line;
   declare
      B : constant One'Class := Pass (A);
   begin
      Put_Line ("---8<---");
      Put_Line (Two'Class (B).M.X'Img);
   end;
   Put_Line ("--->8---");

   New_Line;
   A.M.X := 2;
   declare
      B : One'Class := not A;
   begin
      Put_Line ("---8<---");
      Put_Line (Two'Class (B).M.X'Img);
   end;
   Put_Line ("--->8---");

   New_Line; Put_Line ("Now A is going to finalize");
end Finalize_Leak;
