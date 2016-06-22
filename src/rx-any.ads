with Rx.Definites;

package Rx.Any is

--  When actual values are not important (e.g. with Never, Error, etc), this instance can be used.

   type Any is null record;

   package Instance is new Rx.Definites (Any);

end Rx.Any;
