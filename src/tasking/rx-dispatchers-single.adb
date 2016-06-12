with Rx.Debug;

package body Rx.Dispatchers.Single is

   --------------
   -- Schedule --
   --------------

   overriding procedure Schedule
     (Where : in out Dispatcher;
      What : in out Runnable'Class;
      After : Duration := 0.0)
   is
      use Ada.Calendar;
      Must_Notify : Boolean;
   begin
      Where.Queue.Enqueue (What, Clock + After, Must_Notify);
      if Must_Notify then
         Where.Thread.Notify;
      end if;
   end Schedule;

   ------------
   -- Runner --
   ------------

   task body Runner is
   begin
      loop
         declare
            use Ada.Calendar;
            use Runnable_Holders;
            Exists  : Boolean;
            Ev      : Event;
            Ignore  : Boolean;
         begin
            Parent.Queue.Dequeue (Ev, Exists);
            if Exists then
               select
                  -- An earlier event has arrived, so requeue
                  accept Notify;
                  Parent.Queue.Enqueue (+Ev.Code, Ev.Time, Ignore);
               or
                  delay until Ev.Time; -- This wait may perfectly well be 0
                  Ev.Code.Reference.Run;
               end select;
            else
               select
                  accept Notify;
               or
                  terminate;
               end select;
            end if;
         exception
            when E : others =>
               Debug.Print (E);
         end;
      end loop;
   end Runner;

   ----------
   -- Safe --
   ----------

   protected body Safe is

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue
        (R : Runnable'Class;
         Time : Ada.Calendar.Time;
         Notify : out Boolean)
      is
         use Ada.Calendar;
         use Runnable_Holders;
      begin
         if Queue.Is_Empty or else Queue.Constant_Reference (Queue.First).Time > Time then
            Notify := True;
         end if;
         Queue.Insert ((Time, +R));
      end Enqueue;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue (E : out Event; Exists : out Boolean) is
      begin
         Exists := not Queue.Is_Empty;
         if Exists then
            E := Queue.First_Element;
            Queue.Delete_First;
         end if;
      end Dequeue;

   end Safe;

end Rx.Dispatchers.Single;
