with Ada.Finalization; use Ada.Finalization;
with Ada.Text_Io; use Ada.Text_Io;

procedure Finalize_Leak is
   generic
   package P is

      type One is interface;

      type Managed is new Controlled with null record;
      overriding procedure Finalize (C : in out Managed);

      type Two is new One with record
         M : Managed;
      end record;

   end P;

   package body P is
      overriding procedure Finalize (C : in out Managed) is
         pragma Unreferenced (C);
      begin
         Put_Line ("finalize");
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
   end;
   Put_Line ("--->8---");

   New_Line;
   declare
--      A : Two;
      B : constant One'Class := Pass (A);
   begin
      Put_Line ("---8<---");
   end;
   Put_Line ("--->8---");

   New_Line;
   declare
--      A : Two;
      B : constant One'Class := not A;
   begin
      Put_Line ("---8<---");
   end;
   Put_Line ("--->8---");
   Put_Line ("Now A is going to finalize");
end Finalize_Leak;
