with Ada.Finalization; use Ada.Finalization;
with Ada.Text_Io; use Ada.Text_Io;

procedure Finalize_Leak is
   generic
   package P is

      type Leftie  is interface;
      type Rightie is interface;

      type Holder is new Controlled with record
         Held : access Leftie'Class;
      end record;
      overriding procedure Finalize (H : in out Holder);

      type Subscriber is new Rightie with record
         Parent : Holder;
      end record;
      procedure Set_Parent (S : in out Subscriber; Parent : Leftie'Class);

      type Operator is new Subscriber and Leftie with null record;

      function "&" (L : Leftie'Class; R : Operator'Class) return Operator'Class;

      type Nop is new Operator with null record;

      function N return Operator'Class is (Nop'(others => <>));

   end P;

   package body P is
      overriding procedure Finalize (H : in out Holder) is
         pragma Unreferenced (H);
      begin
         Put_Line ("finalize");
      end Finalize;

      procedure Set_Parent (S : in out Subscriber; Parent : Leftie'Class) is
      begin
         S.Parent := Holder'(Controlled with Held => new Leftie'Class'(Parent));
      end Set_Parent;

      function "&" (L : Leftie'Class; R : Operator'Class) return Operator'Class is
         A : Operator'Class := R;
      begin
         A.Set_Parent (L);
         return A;
      end "&";
   end P;

   package PP is new P; use PP;

begin
   for I in 1 .. 3 loop
      Put_Line ("---8<---");
      declare
         Leak : Operator'Class := N & N;
      begin
         null;
      end;
      Put_Line ("--->8---");
   end loop;
   Put_Line ("END");
   -- Why are there finalizations past this point?
end Finalize_Leak;
