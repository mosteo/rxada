with Rx.Dispatchers.Pools;

package Rx.Schedulers.Pools is

   type Pool is tagged limited private;
   -- Custom pool that will create as much as Size (see below) threads for use

   function Create (Size : Positive; Name : String := "") return Pool;

   function Get_Next (This : in out Pool) return Scheduler;
   --  Round-robin use of threads

   function Get_Idle (This : in out Pool) return Scheduler;
   --  Get first idle (O(N)) thread or next busy

private

   type Pool is new Dispatchers.Pools.Pool with null record;

   function Get_Next (This : in out Pool) return Scheduler is
     (Scheduler (This.Get (Reuse => False)));

   function Get_Idle (This : in out Pool) return Scheduler is
     (Scheduler (This.Find_Idle (Grow => False)));

end Rx.Schedulers.Pools;
