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

   function String_Succ (S : String) return String;
   -- Lexicographic enumeration over the Character type. Useless I guess.

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

   end Images;

   function Interval (First       : Rx_Integer := 0;
                      Period      : Duration := 1.0;
                      First_Pause : Duration := 1.0;
                      Scheduler   : Schedulers.Scheduler := Schedulers.Computation)
                      return Integers.Observable renames Numeric.Integers.Interval;

   function Never return Nothings.Observable renames Nothings.Never;

   function Timer (After : Duration) return Integers.Observable is (Integers.Timer (0, After));
   --  Std Timer emits a 0 after Pause seconds an completes

private

   function String_Succ (S : String) return String
   is (if    S'Length = 0                 then String'(1 => Character'First)
       elsif S (S'Last) /= Character'Last then S (S'First .. S'Last - 1) & Character'Succ (S (S'Last))
       else  S & Character'First);

end Rx.Std;
