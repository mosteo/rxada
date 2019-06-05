with Rx.Dispatchers.Single;

private with Ada.Containers;
private with Ada.Containers.Vectors;
private with Ada.Finalization;

package Rx.Dispatchers.Pools is

   type String_Access is access all String;

   type Pool (Initial_Size : Positive := 8; Name : String_Access := new String'("anonymous"))
   is tagged limited private;

   function Get (From : in out Pool; Reuse : Boolean := False) return Single.Ptr;
   --  In round-robin fashion
   --  If Reuse and last returned thread is idle, return it again

   function Curr_Or_Next (From : in out Pool) return Single.Ptr is (From.Get (Reuse => True));
   --  Return current in pool if idle, or next one regardless of idleness

   function Find_Idle (From : in out Pool; Grow : Boolean := True) return Single.Ptr;
   --  Returns next idle thread, or create one if Grow and none idle (can go beyond initial size)
   --  Returns next non-idle if none idle and not grow
   --  NOTE: THIS IS O(N) in the current implementation

   function New_One (From : in out Pool) return Single.Ptr;
   --  Grows the pool and returns the new one

private

   use type Single.Ptr;

   package Thread_Vectors is new Ada.Containers.Vectors (Positive, Single.Ptr);

   protected type Safe_Pool (Parent : access Pool) is

      procedure Advance;

      procedure Append (Thread : Single.Ptr);
      --  Append a new single y return the current number in the pool
      --  There's potential for race conditions here in which we could end with some extra threads
      --  But it may serve as a simpler implementation

      procedure Curr (Thread : out Single.Ptr);

      procedure Find_Idle (Thread : out Single.Ptr);

      function Exists return Boolean;
      --  Say if the Current thread exists

      function Thread_Count return Natural;

   private
      Threads : Thread_Vectors.Vector;
      Current : Positive := 1;
      Size    : Positive := Parent.Initial_Size;
      --  Max of the actual size or initial size
      --  There might be less than Size created elements in Threads
   end Safe_Pool;

   type Pool (Initial_Size : Positive := 8; Name : String_Access := new String'("anonymous"))
   is new Ada.Finalization.Limited_Controlled with record
      Safe : Safe_Pool (Pool'Access);
   end record;

   function Grow (This : in out Pool) return Single.Ptr;
   --  Adds a thread and returns it, going beyond initial capacity if necessary

   function Ensure_Exists (This : in out Pool) return Single.Ptr;
   -- Returns null if already exists, or else the newly created thread

   overriding procedure Finalize (This : in out Pool);

end Rx.Dispatchers.Pools;
