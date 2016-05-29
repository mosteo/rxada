with Rx.Debug; use Rx.Debug;
with Rx.Observable;

procedure Rx.Test is

   package Strings  is new Rx.Observable (String);
   package Integers is new Rx.Observable (Integer);
   package StrToInt is new Strings.To (Integer);
   package IntToStr is new Integers.To (String);

   function Length (S : String) return Integer is (S'Length);

   use Strings;

begin
   Assemble (
             Strings.Just ("XXX") &
               StrToInt.Map (Length'Access) &
               IntToStr.Map (Integer'Image'Access)
             );

--     Strings.Just ("XXX") &
--       Strings.Map (Length'Access) >
--       Strings.Subscribe;
end Rx.Test;
