private with Ada.Finalization;

generic
   type Item (<>) is limited private;
   type Item_Access is access Item;
package Rx.Shared_Data is

   --  Your typical refcounted access type

   pragma Preelaborate;

   type Proxy is tagged private;

   function Is_Valid (P : Proxy) return Boolean;
   --  True after first call to Wrap

   function Wrap (I : not null Item_Access) return Proxy;

   procedure Apply (P : in out Proxy; CB : access procedure (I : in out Item));

   type Const_Ref (Actual : access constant Item) is limited null record
     with Implicit_Dereference => Actual;

   function Get (P : Proxy) return Const_Ref;
   --  Safe because it cannot outlive the Proxy from which it is retrieved

   type Ref (Actual : access Item) is limited null record
     with Implicit_Dereference => Actual;
   --  UNSAFE unless Item is actually synchronized itself
   --  This should ideally be moved to a separate package with a synchronized interface

   generic
   function Tamper (P : Proxy) return Ref;
   --  This is only safe if Item is in itself thread-safe, otherwise we are
   --  breaking the purpose of the container itself!
   --  Might probably have a specific refcounter for that so it didn't depend on the client well-behavedness
   --  Made generic to raise awareness!

private

   protected type Safe_Item is
      procedure Apply (CB : not null access procedure (I : in out Item));
      function  Get return Const_Ref;
      procedure Set (I : Item_Access);
      function  Get_Count return Natural;
      function  Tamper return Ref; -- Only for synchronized views of Elem.all

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

   function Get    (P : Proxy) return Const_Ref is (P.Safe.Get);
   function Tamper (P : Proxy) return Ref       is (P.Safe.Tamper);

end Rx.Shared_Data;
