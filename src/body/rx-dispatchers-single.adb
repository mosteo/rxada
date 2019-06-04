with Ada.Task_Identification;

with Rx.Debug; use Rx.Debug;

package body Rx.Dispatchers.Single is

   --------------
   -- Schedule --
   --------------

   overriding procedure Schedule
     (Where : in out Dispatcher;
      What  : Runnable'Class;
      Time  : Ada.Calendar.Time := Ada.Calendar.Clock)
   is
      use Ada.Task_Identification;

      Must_Notify : Boolean;
   begin
      Where.Queue.Enqueue (What, Time, Must_Notify);
      if Must_Notify and then Current_Task /= Where.Thread'Identity then
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
         begin
            Parent.Queue.Dequeue (Ev, Exists);
            if Exists and not Dispatchers.Terminating then
               if Ev.Time > Clock then
                  Parent.Queue.Set_Idle (True);
               end if;

               select
                  -- An earlier event has arrived, so requeue
                  accept Notify;
                  Parent.Queue.Enqueue (Ev);
               or
                  delay until Ev.Time; -- This wait may perfectly well be 0

                  Parent.Queue.Set_Idle (False);
                  Ev.Code.Ref.Run;
               end select;
            else
               Parent.Queue.Set_Idle (True);
               select
                  accept Notify;
               or
                  terminate;
               end select;
            end if;
         exception
            when E : others =>
               Debug.Report (E, "At Dispatchers.Single.Runner: ", Debug.Warn, Reraise => False);
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
         Debug.Trace ("enqueue:" & Seq'Img);
         Queue.Insert ((Seq, Time, +R));
         Seq := Seq + 1;
      end Enqueue;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (E : Event) is
      begin
         Queue.Insert (E);
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
            Debug.Trace ("dequeue:" & E.Id'Img);
         end if;
      end Dequeue;

      --------------
      -- Set_Idle --
      --------------

      procedure Set_Idle (Idle : Boolean) is
      begin
         Safe.Idle := Idle;
      end Set_Idle;

      -------------
      -- Is_Idle --
      -------------

      function Is_Idle return Boolean is
      begin
         return Idle;
      end Is_Idle;

   end Safe;

end Rx.Dispatchers.Single;
