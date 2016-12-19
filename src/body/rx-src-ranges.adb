with Rx.Src.Create;

package body Rx.Src.Ranges is

   use Typed.Conversions;

   package Source is new Rx.Src.Create (Typed);

   type Kinds is (Counter, Interval);

   type Observable (Mode : Kinds) is new Source.Observable with record
      Next : Typed.D;
      case Mode is
         when Counter  => Remaining : Rx_Natural;
         when Interval => Last      : Typed.D;
      end case;
   end record;

   overriding procedure On_Subscribe (This : in out Observable; Observer : in out Typed.Subscriber) is
   begin
      case This.Mode is
         when Counter =>
            for I in 1 .. This.Remaining loop
               Observer.On_Next (+This.Next);
               This.Next := +Succ (+This.Next);
            end loop;
         when Interval =>
            while +This.Next < +This.Last or else +This.Next = +This.Last loop
               Observer.On_Next (+This.Next);
               This.Next := +Succ (+This.Next);
            end loop;
      end case;

      Observer.On_Completed;
   end On_Subscribe;

   -------------------
   -- From_Count --
   -------------------

   function From_Count
     (First : Typed.T;
      Count : Rx_Natural)
      return Typed.Observable
   is
      use Typed.Type_Traits;
   begin
      return Source.Tagged_Stateful (Observable'(Mode => Counter, Next => +First, Remaining => Count));
   end From_Count;

   --------------------
   -- From_Slice --
   --------------------

   function From_Slice (First, Last : Typed.T) return Typed.Observable is
   begin
      return Source.Tagged_Stateful (Observable'(Mode => Interval, Next => +First, Last => +Last));
   end From_Slice;

end Rx.Src.Ranges;
