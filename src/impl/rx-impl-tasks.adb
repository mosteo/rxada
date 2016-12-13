with Ada.Unchecked_Deallocation;

package body Rx.Impl.Tasks is

   --------------
   -- Reap_Now --
   --------------

   procedure Reap_Now (This : in out Transient_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation (Transient'Class, Transient_Ptr);
   begin
      if This /= null then
         while not This.all'Terminated loop
            delay 0.01;
         end loop;
         Free (This);
      end if;
   end Reap_Now;

end Rx.Impl.Tasks;
