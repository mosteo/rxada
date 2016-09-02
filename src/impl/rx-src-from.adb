with Rx.Holders;
with Rx.Sources.Stateless;

package body Rx.From is

   ----------------
   -- From_Array --
   ----------------

   package body From_Array is

      package State is new Holders (Arrays.Typed_Array);

      procedure On_Subscribe (S : State.Definite;
                              Consumer : in out Arrays.Typed.Consumers.Observer'Class) is
      begin
         for E of S.CRef loop
            Consumer.On_Next (Arrays.Typed.Type_Traits.To_Indefinite (E));
         end loop;
      end On_Subscribe;

      package Arrayed is new Sources.Stateless (Arrays.Typed, State.Definite, On_Subscribe);

      function From (A : Arrays.Typed_Array) return Arrays.Typed.Producers.Observable'Class
      is
      begin
         return Arrayed.Create (State.Hold (A));
      end From;

   end From_Array;

   -------------------
   -- From_Iterable --
   -------------------

   package body From_Iterable is

      procedure On_Subscribe (State    : Iterable.Cursor;
                              Consumer : in out Iterable.Typed.Consumers.Observer'Class)
      is
         use Iterable;
         I : Cursor := State;
      begin
         while Has_Element (I) loop
            Consumer.On_Next (Element (I));
            I := Next (I);
         end loop;
      end On_Subscribe;

      package Iterables is new Rx.Sources.Stateless (Iterable.Typed, Iterable.Cursor, On_Subscribe);

      function From (C : Iterable.Container) return Iterable.Typed.Producers.Observable'Class is
      begin
         return Iterables.Create (Iterable.First (C));
      end From;

   end From_Iterable;

end Rx.From;
