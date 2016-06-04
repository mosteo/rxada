with Rx.Debug;

procedure Rx.Examples.Basic is
   use Integers;

   O : constant Integers.Producers.Observable'Class := Just (1);

begin
   null;
exception
   when E : others =>
      Debug.Print (E);
end Rx.Examples.Basic;
