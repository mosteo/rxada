with Ada.Unchecked_Deallocation;

with Rx.Debug;

package body Rx.Impl.Tasks is

   overriding procedure Initialize (This : in out Reaper) is
      procedure Free is new Ada.Unchecked_Deallocation (Transient'Class, Transient_Ptr);
      Victim : Transient_Ptr := This.Victim;
   begin
      Free (Victim);
   exception
      when E : others =>
         Debug.Print (E);
   end Initialize;

end Rx.Impl.Tasks;
