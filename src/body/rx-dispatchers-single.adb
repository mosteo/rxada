with Ada.Task_Identification;

with Rx.Debug; use Rx.Debug;

package body Rx.Dispatchers.Single is

   --------------
   -- Sequence --
   --------------

   protected Sequence is
      procedure Next (Id : out Long_Long_Integer);
   private
      Curr : Long_Long_Integer := 1;
   end Sequence;

   protected body Sequence is
      procedure Next (Id : out Long_Long_Integer) is
      begin
         Id := Curr;
         Curr := Curr + 1;
      end Next;
   end Sequence;

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
         Debug.Trace ("schedule: notifying");
         Where.Thread.Notify;
         Debug.Trace ("schedule: notified");
      else
         Debug.Trace ("schedule: not notifying");
      end if;
   end Schedule;

   ------------
   -- Runner --
   ------------

   task body Runner is
      Runner_Id : Long_Long_Integer := -1;
      function Runner_Addr return String is ("#" & Runner_Id'Img);
   begin
      Sequence.Next (Runner_Id);
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

               Debug.Trace ("runner [waiting] " & Runner_Addr);
               select
                  -- An earlier event has arrived, so requeue
                  accept Notify;
                  Parent.Queue.Enqueue (Ev);
               or
                  delay until Ev.Time; -- This wait may perfectly well be 0

                  Parent.Queue.Set_Idle (False);
                  Debug.Trace ("runner [running] " & Runner_Addr);
                  Ev.Code.Ref.Run;
                  Debug.Trace ("runner [ran] " & Runner_Addr);
               end select;
            else
               Debug.Trace ("runner [idling] " & Runner_Addr);
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
         Queue.Insert ((Seq, Time, +R));
         Debug.Trace ("enqueue:" & Seq'Img & " (" & Queue.Length'Img & ") #" & Parent.Addr_Img);
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
            Debug.Trace ("dequeue:" & E.Id'Img & " (" & Queue.Length'Img & ") #" & Parent.Addr_Img);
         else
            Debug.Trace ("dequeue [empty]" & " (" & Queue.Length'Img & ") #" & Parent.Addr_Img);
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
