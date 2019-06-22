with Ada.Task_Identification;

with Rx.Debug; use Rx.Debug;

package body Rx.Dispatchers.Single is

   -------------
   -- Is_Idle --
   -------------

   function Is_Idle (This : in out Dispatcher) return Boolean is (This.Idle);

   --------------
   -- Schedule --
   --------------

   overriding procedure Schedule
     (Where : in out Dispatcher;
      What  : Runnable'Class;
      Time  : Ada.Calendar.Time := Ada.Calendar.Clock)
   is
   begin
      Where.Queue.Enqueue (What, Time);
      --  This must succeed sooner than later
   end Schedule;

   ------------
   -- Queuer --
   ------------

   task body Queuer is
      use Ada.Calendar;
      use Ada.Task_Identification;
      function Addr return String is ("@" & Image (Current_Task) & " ");

      function Min (L, R : Time) return Time is (if L < R
                                                 then L
                                                 else R);

      use Runnable_Holders;
      Queue : Event_Queues.Set;
      Seq   : Event_Id := 1;
      Await : Boolean  := False;
   begin
      loop
         begin
            --  Block when idle, task already running, or forced shutdown
            if Await or else Queue.Is_Empty or else Dispatchers.Terminating then
               Debug.Trace ("queuer [terminable] (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
               select
                  accept Enqueue (R : Runnable'Class; Time : Ada.Calendar.Time) do
                     Queue.Insert ((Seq, Time, +R));
                  end Enqueue;
                  Parent.Length := Natural (Queue.Length);
                  Debug.Trace ("queuer [enqueue]:" & Seq'Img & " (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
                  Seq := Seq + 1;
               or
                  accept Reap;
                  Await := False;
                  Debug.Trace ("queuer [reaped] (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
               or
                  terminate;
               end select;
            end if;

            --  If idle and pending tasks, try to run one
            if not Await and then not Queue.Is_Empty and then not Dispatchers.Terminating then
               declare
                  Ev : constant Event := Queue.First_Element;
               begin
                  Queue.Delete_First;
                  if Ev.Time <= Clock then
                     --  Try execution
                     select
                        Parent.Thread.Run (Ev.Code);
                        Await := True;
                        Parent.Length := Natural (Queue.Length);
                        Debug.Trace ("queuer [dequeued] delta:" & Duration'Image (Ev.Time - Clock) & " id:"
                                     & Ev.Id'Img & " (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
                     else
                        Queue.Insert (Ev); -- Requeue failed run
                        Debug.Trace ("queuer [busy] ev" & Ev.Id'Img);
                     end select;
                  else
                     Debug.Trace ("queuer [future] delta:" & Duration'Image (Ev.Time - Clock) & " id:"
                                  & Ev.Id'Img & " (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
                     Queue.Insert (Ev); -- Requeue future event
                  end if;

                  --  Block when idle but event incoming
                  if not Await then -- Otherwise we just ran the event!
                     Debug.Trace ("queuer [pending] delta:" & Duration'Image (Ev.Time - Clock)
                                  & " (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
                     select
                        accept Enqueue (R : Runnable'Class; Time : Ada.Calendar.Time) do
                           Queue.Insert ((Seq, Time, +R));
                        end Enqueue;
                        Parent.Length := Natural (Queue.Length);
                        Debug.Trace ("queuer [enqueue]:" & Seq'Img & " (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
                        Seq := Seq + 1;
                     or
                        delay until Min (Ev.Time, Clock + 1.0);
                        Debug.Trace ("queuer [break delta:" & Duration'Image (Ev.Time - Clock)
                                     & "] (" & Queue.Length'Img & ") " & Addr & Parent.Addr_Img);
                        --  Periodically break to check for global termination
                        --  Note that when we are past deadline this task will be
                        --    100% busy
                     end select;
                  end if;
               end;
            end if;
         exception
            when E : others =>
               Debug.Report (E, "Dispatchers.Single.Queuer: ", Debug.Warn, Reraise => False);
         end;
      end loop;
   end Queuer;

   ------------
   -- Runner --
   ------------

   task body Runner is
      use Ada.Task_Identification;
      function Addr return String is ("@" & Image (Current_Task) & " ");
   begin
      loop
         declare
            RW : Runnable_Def;
         begin
            Debug.Trace ("runner [ready] " & Addr & Parent.Addr_Img);
            Parent.Idle := True;
            select
               accept Run (R : Runnable_Def) do
                  Parent.Idle := False;
                  RW := R;
               end Run;
            or
               terminate;
            end select;

            Debug.Trace ("runner [running] " & Addr & Parent.Addr_Img);
            begin
               RW.Ref.Run;
            exception
               when E : others =>
                  Debug.Report (E, "Dispatchers.Single.Runner.Run: ", Debug.Warn);
            end;
            Parent.Queue.Reap;
         exception
            when E : others =>
               Debug.Report (E, "Dispatchers.Single.Runner: ", Debug.Warn, Reraise => False);
         end;
      end loop;
   end Runner;

end Rx.Dispatchers.Single;
