with Rx.Operator;
with Rx.Schedulers;

package Rx.Interval is

   pragma Elaborate_Body;

   generic
      Pause       : Duration := 1.0;
      First_Pause : Duration := Pause;
      Scheduler   : Rx.Schedulers.Object := Rx.Schedulers.Background;
   package Producer is

      package Output is new Rx.Operator (Integer);

   private

      type Observable is new Output.Observable with record
         Next : Positive := 1;
      end record;

      overriding
      procedure Subscribe (O : in out Observable;
                           S : access Output.Observer'Class);

   end Producer;

--  private
--
--     package Base is new Rx.Operator (Positive);
--
--     type Observable is abstract new Base.Observable with record
--        Next : Positive := 1;
--     end record;
--
--     overriding
--     procedure Subscribe (O : in out Observable;
--                          S : access Base.Observer'Class);

end Rx.Interval;
