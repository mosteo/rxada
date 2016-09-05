private with Ada.Finalization;

generic
   type Item (<>) is limited private;
   type Item_Access is access Item;
package Rx.Shared_Data is

   pragma Preelaborate;

   type Proxy is tagged private;

   function Is_Valid (P : Proxy) return Boolean;
   --  True after first call to Wrap

   function Wrap (I : not null Item_Access) return Proxy;

   procedure Apply (P : in out Proxy; CB : access procedure (I : in out Item));

   type Const_Ref (Actual : access constant Item) is limited private
     with Implicit_Dereference => Actual;

   function Get (P : Proxy) return Const_Ref;

private

   protected type Safe_Item is
      procedure Apply (CB : not null access procedure (I : in out Item));
      function  Get return Const_Ref;
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

   function Is_Valid (P : Proxy) return Boolean is (P.Safe /= null);

   type Const_Ref (Actual : access constant Item) is limited null record;

end Rx.Shared_Data;
