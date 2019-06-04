with Ada.Exceptions;

with Rx.Errors;
with Rx.Impl.Casts;
with Rx.Impl.Std;
with Rx.Observables.Image;
with Rx.Subscriptions;
with Rx.Schedulers;

package Rx.Std is

   --  Instances and default visibility for some predefined types
   --  Also default sources/operators from ReactiveX documentation

   --  Note that default integers are not Standard.Integer but the largest integer type supported

   --  Type shortcuts:

   subtype Subscription is Rx.Subscriptions.Subscription;

   --  Convenience instances

   --  Base Std types

   package Integers renames Impl.Std.Integers.Observables;
   package Floats   renames Impl.Std.Floats.Observables;
   package Strings  renames Impl.Std.Strings.Observables;
   package Nothings renames Impl.Std.Nothings.Observables;

   --  Numeric self-operations for base types

   package Numeric renames Impl.Std.Numeric;

   --  Transforming operators between base types

   package Integer_To_Float  renames Impl.Std.Int_To_Float;
   package String_To_Float   renames Impl.Std.String_To_Float;

   package Float_To_Integer  renames Impl.Std.Float_To_Integer;
   package String_To_Integer renames Impl.Std.String_To_Integer;

   package Float_To_String   renames Impl.Std.Float_To_String;
   package Integer_To_String renames Impl.Std.Integer_To_String;

   subtype Printable_Character is Character range Character'Val (16#20#) .. Character'Val (16#7E#);

   function String_Succ    (S : Rx_String) return Rx_String;
   function Printable_Succ (S : Rx_String) return Rx_String; -- with
--       Pre  => (for all C of S                     => C in Printable_Character),
--       Post => (for all C of Printable_Succ'Result => C in Printable_Character);
   -- Lexicographic enumerations over the Character type. Useless I guess.

   function All_Integers (Initial : Rx_Integer := Rx_Integer'First;
                          Count   : Rx_Integer := Rx_Integer'Last) return Integers.Observable;

   function All_Naturals (Count   : Rx_Integer := Rx_Integer'Last) return Integers.Observable;

   function All_Positives (Count   : Rx_Integer := Rx_Integer'Last) return Integers.Observable;

   --  See also Rx.Src.Ranges

   function All_Strings (Initial : Rx_String  := "";
                         Count   : Rx_Integer := Rx_Integer'Last) return Strings.Observable;

   function All_Printable_Strings (Initial : Rx_String  := "";
                                   Count   : Rx_Integer := Rx_Integer'Last) return Strings.Observable; -- with
--       Pre => (for all C of Initial => C in Printable_Character);

   --  Standard Rx sources and operators

   package Casts is

      --  Casts for predefined types

      function To_Float return Integer_To_Float.Operator is (Integer_To_Float.Map (Rx.Impl.Casts.To_Float'Access));
      function To_Float return String_To_Float.Operator  is (String_To_Float.Map (Rx.Impl.Casts.To_Float'Access));

      function To_Integer return Float_To_Integer.Operator  is (Float_To_Integer.Map (Rx.Impl.Casts.To_Integer'Access));
      function To_Integer return String_To_Integer.Operator is (String_To_Integer.Map (Rx.Impl.Casts.To_Integer'Access));

      function To_String return Float_To_String.Operator   is (Float_To_String.Map (Rx.Impl.Casts.To_String'Access));
      function To_String return Integer_To_String.Operator is (Integer_To_String.Map (Rx.Impl.Casts.To_String'Access));

   end Casts;

   function Empty return Nothings.Observable renames Nothings.Empty;

   function Error (E : Rx.Errors.Occurrence)                return Nothings.Observable renames Nothings.Error;
   function Error (E : Ada.Exceptions.Exception_Occurrence) return Nothings.Observable renames Nothings.Error;

   package Images is

      package Floats   is new Std.Floats.Image   (Impl.Casts.To_String);
      package Integers is new Std.Integers.Image (Impl.Casts.To_String);
      package Strings  is new Std.Strings.Image  (Impl.Casts.To_String);

   end Images;

   function Interval (First       : Rx_Integer := 1;
                      Period      : Duration := 1.0;
                      First_Pause : Duration := 0.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable renames Numeric.Integers.Interval;

   function Never return Nothings.Observable renames Nothings.Never;

   function Timer (After : Duration) return Integers.Observable is (Integers.Timer (0, After));
   --  Std Timer emits a 0 after Pause seconds an completes

private

   function Succ (Head, Tail : String; Printable : Boolean) return String is
     (if Head = "" then (1 .. Tail'Length + 1 => (if Printable then Printable_Character'First else Character'First))
      elsif Head (Head'Last) < (if Printable then Printable_Character'Last else Character'Last) then
           Head (Head'First .. Head'Last - 1) & (if Printable
                                                 then Printable_Character'Succ (Head (Head'Last))
                                                 else Character'Succ (Head (Head'Last))) & Tail
      else Succ (Head (Head'First .. Head'Last - 1),
                 String'(1 .. Tail'Length + 1 => (if Printable then Printable_Character'First else Character'First)),
                 Printable));


   function String_Succ (S : Rx_String) return Rx_String is
     (Succ (S, "", False));

   function Printable_Succ (S : Rx_String) return Rx_String is
     (Succ (S, "", True));

   function Succ (I : Rx_Integer) return Rx_Integer is (Rx_Integer'Succ (I));

   function All_Integers (Initial : Rx_Integer := Rx_Integer'First;
                          Count   : Rx_Integer := Rx_Integer'Last) return Integers.Observable is
      (Integers.Create (Initial, Succ'Access, Count));

   function All_Naturals (Count   : Rx_Integer := Rx_Integer'Last) return Integers.Observable is
      (All_Integers (Initial => 0, Count => Count));

   function All_Positives (Count   : Rx_Integer := Rx_Integer'Last) return Integers.Observable is
      (All_Integers (Initial => 1, Count => Count));

   function All_Strings (Initial : Rx_String  := "";
                         Count   : Rx_Integer := Rx_Integer'Last) return Strings.Observable is
      (Strings.Create (Initial, String_Succ'Access, Count));

   function All_Printable_Strings (Initial : Rx_String  := "";
                                   Count   : Rx_Integer := Rx_Integer'Last) return Strings.Observable is
      (Strings.Create (Initial, Printable_Succ'Access, Count));

end Rx.Std;
