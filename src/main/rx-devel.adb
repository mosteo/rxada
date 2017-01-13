procedure Rx.Devel is

   type Base is tagged null record;

   type Child is new Base with record
      X : Integer;
   end record;

   B : Base;

   C : Child;

begin
   C.X := 7;
   B   := Base (C);
end Rx.Devel;
