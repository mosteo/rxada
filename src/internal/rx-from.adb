with Rx.Sources.Stateless;

package body Rx.From is

   ----------------
   -- From_Array --
   ----------------

   package body From_Array is

      type Observable (First, Last : Index_Type) is new Typed.Producers.Observable with record
         Values : Array_Type (First .. Last);
      end record;

      overriding
      procedure Subscribe (Producer : in out Observable;
                           Consumer : in out Typed.Consumers.Observer'Class) is
      begin
         for E of Producer.Values loop
            Consumer.On_Next (Typed.Type_Traits.To_Indefinite (E));
         end loop;
         Consumer.On_Completed;
      end Subscribe;

      ----------
      -- From --
      ----------

      function From
        (A : Array_Type)
         return Typed.Producers.Observable'Class
      is
      begin
         return Observable'(First => A'First, Last => A'Last, Values => A);
      end From;

   end From_Array;

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
