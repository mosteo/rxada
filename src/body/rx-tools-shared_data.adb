with Ada.Unchecked_Deallocation;

package body Rx.Tools.Shared_Data is

   ----------
   -- Wrap --
   ----------

   function Wrap (I : not null Item_Access) return Proxy is
   begin
      return P : Proxy do
         P.Safe := new Safe_Item;
         P.Safe.Set (I);
      end return;
   end Wrap;

   -----------
   -- Apply --
   -----------

   procedure Apply (P : in out Proxy; CB : access procedure (I : in out Item)) is
   begin
      P.Safe.Apply (CB);
   end Apply;

   ------------
   -- Forget --
   ------------

   procedure Forget (P : in out Proxy) is
      Is_Last : Boolean;
   begin
      P.Safe.Forget (Is_Last);
      if Is_Last then
         P.Finalize;
      else
         P.Safe := null;
      end if;
   end Forget;

   ------------
   -- Tamper --
   ------------

   function Tamper (P : Proxy) return Ref       is (P.Safe.Tamper);

   ---------------
   -- Safe_Item --
   ---------------

   protected body Safe_Item is

      -----------
      -- Apply --
      -----------

      procedure Apply (CB : not null access procedure (I : in out Item)) is
      begin
         CB (Elem.all);
      end Apply;

      ---------
      -- Set --
      ---------

      procedure Set (I : Item_Access) is
      begin
         Elem := I;
      end Set;

      ------------
      -- Forget --
      ------------

      procedure Forget (Is_Last : out Boolean) is
      begin
         if Count > 0 then
            Finalize;
            Is_Last := Get_Count = 0;
         else
            raise Constraint_Error;
         end if;
      end Forget;

      ---------
      -- Get --
      ---------

      function Get return Const_Ref is
      begin
         return Const_Ref'(Actual => Elem);
      end Get;

      ---------------
      -- Get_Count --
      ---------------

      function Get_Count return Natural is
      begin
         return Count;
      end Get_Count;

      ------------
      -- Adjust --
      ------------

      procedure Adjust is
      begin
         Count := Count + 1;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
         procedure Free is new Ada.Unchecked_Deallocation (Item, Item_Access);
      begin
         Count := Count - 1;
         if Count = 0 then
            Free (Elem);
         end if;
      end Finalize;

      ------------
      -- Tamper --
      ------------

      function Tamper return Ref is
      begin
         return Ref'(Actual => Elem);
      end Tamper;

   end Safe_Item;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (P : in out Proxy) is
   begin
      if P.Safe /= null then
         P.Safe.Adjust;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (P : in out Proxy) is
      procedure Free is new Ada.Unchecked_Deallocation (Safe_Item, Safe_Access);
   begin
      if P.Safe /= null then
         P.Safe.Finalize;
         if P.Safe.Get_Count = 0 then
            Free (P.Safe);
         end if;
      end if;
   end Finalize;

end Rx.Tools.Shared_Data;
