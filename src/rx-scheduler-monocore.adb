with Ada.Real_Time;

with Rx.Debug;

package body Rx.Scheduler.Monocore is

   protected body Safe is

      procedure OnEvent (Event : in out TE.Timing_Event) is
         RE : Runnable'Class renames Runnable'Class (TE.Timing_Event'Class (Event));
      begin
         Debug.Put_Line ("RING!");
         Queue.Append (RE'Unchecked_Access);
      end OnEvent;

      entry Get (R : out Runnable_Access)
        when not Queue.Is_Empty
      is
      begin
         R := Queue.First_Element;
         Queue.Delete_First;
      end Get;
   end Safe;

   --------------
   -- Schedule --
   --------------

   overriding
   procedure Schedule (Where : in out Object; What : in out Runnable'Class; After : Duration := 0.0)
   is
   begin
      TE.Set_Handler (TE.Timing_Event (What),
                      Ada.Real_Time.To_Time_Span (After),
                      Where.Queue.OnEvent'Unrestricted_Access); -- Shouldn't be a problem... as long as de Scheduler is not going to dissapear on us?
   end Schedule;

   ------------
   -- Runner --
   ------------

   task body Runner is
      R : Runnable_Access;
   begin
      loop
         begin
            Debug.Put_Line ("About to run 1");
            Parent.Queue.Get (R);
            Debug.Put_Line ("About to run 2");
            R.Run;
            Debug.Put_Line ("About to run 3");
         exception
            when E : others =>
               Debug.Put_Line ("UH OH...");
               Debug.Print (E);
         end;
      end loop;
   end Runner;

end Rx.Scheduler.Monocore;
