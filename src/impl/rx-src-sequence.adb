with Rx.Src.Create;

package body Rx.Src.Sequence is

   package Creators is new Src.Create (Typed);

   type Source is new Creators.Observable with record
      First : Typed.D;
      Count : Natural;
   end record;

   overriding
   procedure On_Subscribe (This : in out Source; Observer : in out Typed.Subscriber) is
      Curr : Typed.D := This.First;
      use Typed.Type_Traits;
   begin
      for I in 1 .. This.Count loop
         Observer.On_Next (+Curr);
         Curr := +Succ (+Curr);
      end loop;

      Observer.On_Completed;
   end On_Subscribe;

   ------------
   -- Create --
   ------------

   function Create
     (First : Typed.T;
      Count : Natural)
      return Typed.Observable
   is
      use Typed.Type_Traits;
   begin
      return Creators.Tagged_Stateful (Source'(First => +First, Count => Count));
   end Create;

end Rx.Src.Sequence;
