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

end Rx.From;
