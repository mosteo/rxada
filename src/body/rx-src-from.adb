with Rx.Debug;
with Rx.Holders;
with Rx.Src.Create;
with Rx.Subscriptions;

package body Rx.Src.From is

   ----------------
   -- From_Array --
   ----------------

   package body From_Array is

      package Create is new Src.Create (Arrays.Typed);

      package State is new Holders (Arrays.Typed_Array);

      procedure On_Subscribe (S : State.Definite;
                              Consumer : in out Arrays.Typed.Subscriber) is
      begin
         for E of S.CRef loop
            if Consumer.Is_Subscribed then
               Consumer.On_Next (Arrays.Typed.Type_Traits.To_Indefinite (E));
            else
               exit;
            end if;
         end loop;
      end On_Subscribe;

      package Arrayed is new Create.With_State (State.Definite, On_Subscribe);

      function From (A : Arrays.Typed_Array) return Arrays.Typed.Contracts.Observable'Class
      is
      begin
         return Arrayed.Create (State.Hold (A));
      end From;

   end From_Array;

   -------------------
   -- From_Iterable --
   -------------------

   package body From_Iterable is

      package Create is new Src.Create (Iterable.Typed);

      procedure On_Subscribe (State    : Iterable.Container;
                              Consumer : in out Iterable.Typed.Subscriber)
      is
         use Iterable;
         procedure For_Each (V : Iterable.Typed.T) is
         begin
            if Consumer.Is_Subscribed then
               Consumer.On_Next (V);
            else
               raise Subscriptions.No_Longer_Subscribed;
            end if;
         end For_Each;
      begin
         Iterable.Iterate (State, For_Each'Access);
      exception
         when Subscriptions.No_Longer_Subscribed =>
            Debug.Log ("From_Iterable: caught No_Longer_Subscribed", Debug.Note);
      end On_Subscribe;

      package Iterables is new Create.With_State (Iterable.Container, On_Subscribe);

      function From (C : Iterable.Container) return Iterable.Typed.Contracts.Observable'Class is
      begin
         return Iterables.Create (C);
      end From;

   end From_Iterable;

end Rx.Src.From;
