private with Ada.Finalization;

generic
   type Item (<>) is limited private;
   type Item_Access is access Item;
   Debug_Name : String := "anon";
package Rx.Tools.Shared_Data with Preelaborate is

   --  Your typical refcounted thread-safe access type

   type Const_Ref (Actual : access constant Item) is limited private
     with Implicit_Dereference => Actual;

   type Ref (Actual : access Item) is limited private
     with Implicit_Dereference => Actual;
   --  UNSAFE unless Item is actually synchronized itself
   --  This should ideally be moved to a separate package with a synchronized interface

   type Proxy is tagged private;

   function Is_Valid (P : Proxy) return Boolean;
   --  True after first call to Wrap

   function Wrap (I : not null Item_Access) return Proxy;

   procedure Forget (P : in out Proxy)
     with Pre => P.Is_Valid or else raise Program_Error;
   --  Invalidate this proxy and decrease refcount

   procedure Apply (P : in out Proxy; CB : access procedure (I : in out Item))
     with Pre => P.Is_Valid or else raise Program_Error;
   --  This takes place inside a protected object! So no blocking calls in procedure...

   function Get (P : Proxy) return Const_Ref
     with Pre => P.Is_Valid or else raise Program_Error;
   --  Safe because it cannot outlive the Proxy from which it is retrieved

   generic
      --  WATCH WHATCHA DOIN'!!
   function Tamper (P : Proxy) return Ref
     with Pre => P.Is_Valid or else raise Program_Error;
   --  This is only safe if Item is in itself thread-safe, otherwise we are
   --  breaking the purpose of the container itself!
   --  Might probably have a specific refcounter for that so it didn't depend on the client well-behavedness
   --  Made generic to raise awareness!

private

   protected type Safe_Item is
      procedure Apply (Elem : Item_Access; CB : not null access procedure (I : in out Item));
      procedure Forget (Is_Last : out Boolean);
      procedure Adjust;
      procedure Finalize (Remain : out Natural); -- Says how many remain
   private
      Count : Natural := 1;
   end Safe_Item;

   type Safe_Access is access Safe_Item;

   type Proxy is new Ada.Finalization.Controlled with record
      Safe : Safe_Access;
      Item : Item_Access;
   end record;

   overriding procedure Adjust   (P : in out Proxy);
   overriding procedure Finalize (P : in out Proxy);

   function Is_Valid (P : Proxy) return Boolean is (P.Safe /= null);

   ---------------
   -- References -
   ---------------

   type Const_Ref (Actual : access constant Item) is limited record
      Self : Proxy; -- Taken to ensure Item is not freed as long as refs remain
                    --  See Ada Gem #107
   end record;

   type Ref (Actual : access Item) is limited record
      Self : Proxy; -- Taken to ensure Item is not freed as long as refs remain
                    --  See Ada Gem #107
   end record;

   function Get (P : Proxy) return Const_Ref is
     (Actual => P.Item,
      Self   => P);

end Rx.Tools.Shared_Data;
