private with Ada.Finalization;

generic
   type Item (<>) is limited private;
   type Item_Access is access Item;
package Rx.Shared_Data is

   pragma Preelaborate;

   type Proxy (<>) is tagged private;

   function Wrap (I : not null Item_Access) return Proxy;

   procedure Apply (P : in out Proxy; CB : access procedure (I : in out Item));

private

   protected type Safe_Item is
      procedure Apply (CB : not null access procedure (I : in out Item));
      procedure Set (I : Item_Access);
      function  Get_Count return Natural;

      procedure Adjust;
      procedure Finalize;
   private
      Count : Natural := 1;
      Elem  : Item_Access;
   end Safe_Item;

   type Safe_Access is access Safe_Item;

   type Proxy is new Ada.Finalization.Controlled with record
      Safe : Safe_Access;
   end record;

   overriding procedure Adjust   (P : in out Proxy);
   overriding procedure Finalize (P : in out Proxy);

end Rx.Shared_Data;
