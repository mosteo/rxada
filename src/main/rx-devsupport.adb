package body Rx.Devsupport is

   -------------
   -- Blah123 --
   -------------

   procedure Blah123 (Observer : in out Std.Integers.Typed.Observer'Class) is
   begin
      Observer.On_Next (1);
      delay 0.2;
      Observer.On_Next (2);
      delay 0.2;
      Observer.On_Next (3);
      Observer.On_Next (4);
      Observer.On_Next (5);
      Observer.On_Complete ;
   end Blah123;

end Rx.Devsupport;
