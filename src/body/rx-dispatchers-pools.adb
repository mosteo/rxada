with Rx.Debug;

package body Rx.Dispatchers.Pools is

   ----------
   -- Grow --
   ----------

   function Grow (This : in out Pool) return Single.Ptr is
      Last_Created : constant Single.Ptr := new Single.Dispatcher;
   begin
      This.Safe.Append (Last_Created);
      return Last_Created;
   end Grow;

   -------------------
   -- Ensure_Exists --
   -------------------

   function Ensure_Exists (This : in out Pool) return Single.Ptr is
      Last_Created : Single.Ptr;
   begin
      while not This.Safe.Exists loop
         Last_Created := This.Grow;
      end loop;

      return Last_Created;
   end Ensure_Exists;

   ---------
   -- Get --
   ---------

   function Get (From : in out Pool; Reuse : Boolean := False) return Single.Ptr is
      Result : Single.Ptr;
   begin
      Result := From.Ensure_Exists;

      if Reuse then
         if Result /= null then
            return Result; -- Just created, hence idle
         else
            From.Safe.Curr (Result);
            if Result.Is_Idle then
               return Result;
            end if;
         end if;
      end if;

      From.Safe.Advance;
      Result := From.Ensure_Exists;
      if Result /= null then
         return Result;
      else
         From.Safe.Curr (Result);
         return Result;
      end if;

   end Get;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Pool) is
   begin
      Debug.Log ("Thread pool "  & This.Name.all & " finalizing with" & This.Safe.Thread_Count'Img &
                   " live threads", Debug.Note);
   end Finalize;

   ---------------
   -- Find_Idle --
   ---------------

   function Find_Idle (From : in out Pool; Grow : Boolean := True) return Single.Ptr is
      Result : Single.Ptr;
   begin
      From.Safe.Find_Idle (Result);
      if Result /= null then
         return Result;
      else
         if Grow then
            return From.Grow;
         else
            return From.Get (Reuse => False);
         end if;
      end if;
   end Find_Idle;

   -------------
   -- New_One --
   -------------

   function New_One (From : in out Pool) return Single.Ptr is
   begin
      return From.Grow;
   end New_One;


   protected body Safe_Pool is

      -------------
      -- Advance --
      -------------

      procedure Advance is
      begin
         Current := Current + 1;
         if Current > Size then
            Current := 1;
         end if;
      end Advance;

      ------------
      -- Append --
      ------------

      procedure Append (Thread : Single.Ptr) is
      begin
         Threads.Append (Thread);
         Size := Natural'Max (Size, Natural (Threads.Length));
      end Append;

      ----------
      -- Curr --
      ----------

      procedure Curr (Thread : out Single.Ptr) is
      begin
         Thread := Threads.Element (Current);
      end Curr;

      ---------------
      -- Find_Idle --
      ---------------

      procedure Find_Idle (Thread : out Single.Ptr) is
      begin
         -- Look for Idle one
         for T of Threads loop
            if T.Is_Idle then
               Thread := T;
               return;
            end if;
         end loop;

         -- There was none
         Thread := null;
      end Find_Idle;

      ------------
      -- Exists --
      ------------

      function Exists return Boolean is
      begin
         return Natural (Threads.Length) >= Current;
      end Exists;

      ------------------
      -- Thread_Count --
      ------------------

      function Thread_Count return Natural is
      begin
         return Natural (Threads.Length);
      end Thread_Count;

   end Safe_Pool;

end Rx.Dispatchers.Pools;
