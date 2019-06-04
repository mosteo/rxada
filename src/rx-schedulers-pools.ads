with Rx.Dispatchers.Pools;

package Rx.Schedulers.Pools is

   type Pool (<>) is limited new Schedulers.Pool with private;
   -- Custom pool that will create as much as Size (see below) threads for use

   function Create (Size : Positive; Name : String := "") return Pool;

   function Get_Next (This : in out Pool) return Thread;
   --  Round-robin use of threads

   function Get_Idle (This : in out Pool) return Thread;
   --  Get first idle (O(N)) thread or next busy

   overriding function Get_Thread (This : in out Pool) return Thread
     renames Get_Next;

private

   type Pool is limited new Dispatchers.Pools.Pool and Schedulers.Pool with null record;

   function Get_Next (This : in out Pool) return Thread is
     (Thread (This.Get (Reuse => False)));

   function Get_Idle (This : in out Pool) return Thread is
     (Thread (This.Find_Idle (Grow => False)));

end Rx.Schedulers.Pools;
