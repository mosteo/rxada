with Ada.Real_Time;

package body Rx.Scheduler is

   type Run_Event is new TE.Timing_Event with record
      R : Runnable_Holder;
      S : Object_Access;
   end record;

   protected body Safe is

      procedure OnEvent (Event : in out TE.Timing_Event) is
         RE : Run_Event renames Run_Event (TE.Timing_Event'Class (Event));
      begin
         Queue.Append (RE.R.Element);
      end OnEvent;

      entry Get (R : out Runnable_Holder)
        when not Queue.Is_Empty
      is
      begin
         R := To_Holder (Queue.First_Element);
         Queue.Delete_First;
      end Get;
   end Safe;

   --------------
   -- Schedule --
   --------------

   procedure Schedule
     (Where : in out Object;
      After : Duration;
      What  : Runnable'Class)
   is
      E : Run_Event;
   begin
      E.R := To_Holder (What);
      E.S := Where.Queue.Parent;
      TE.Set_Handler (TE.Timing_Event (E),
                      Ada.Real_Time.To_Time_Span (After),
                      Where.Queue.OnEvent'Unrestricted_Access); -- Shouldn't be a problem... as long as de Scheduler is not going to dissapear on us?
   end Schedule;

   ------------
   -- Runner --
   ------------

   task body Runner is
      R : Runnable_Holder;
   begin
      loop
         begin
            Parent.Queue.Get (R);
            R.Constant_Reference.Run;
         exception
            when E : others =>
               null;
         end;
      end loop;
   end Runner;

end Rx.Scheduler;
