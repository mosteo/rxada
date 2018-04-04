with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Text_Io; use Ada.Text_Io;

procedure Finalize_Leak is
   generic
   package P is

      type Leftie  is interface;
      type Rightie is interface;

      type Left_Access is access Leftie'Class;
      type Holder is new Controlled with record
         Held : Left_Access;
      end record;
      overriding procedure Adjust (Op : in out Holder);
      overriding procedure Finalize (Op : in out Holder);
      function Hold (L : Leftie'Class) return Holder
        is (Holder'(Controlled with Held => new Leftie'Class'(L)));

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
      overriding procedure Adjust (Op : in out Holder) is
      begin
         if Op.Held /= null then
            Put_Line ("adjust");
            Op.Held := new Leftie'Class'(Op.Held.all);
         end if;
      end Adjust;
      overriding procedure Finalize (Op : in out Holder) is
         procedure Free is new Ada.Unchecked_Deallocation (Leftie'Class, Left_Access);
      begin
         Put_Line ("finalize");
         Free (Op.Held);
      end Finalize;

      procedure Set_Parent (S : in out Subscriber; Parent : Leftie'Class) is
      begin
         S.Parent := Hold (Parent);
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
   for I in 1 .. 1 loop
      Put_Line ("---8<---");
      declare
         Leak : Leftie'Class := N & N with Unreferenced;
      begin
         null;
      end;
      Put_Line ("--->8---");
   end loop;
   Put_Line ("END");
   -- Why are there finalizations past this point?
end Finalize_Leak;
