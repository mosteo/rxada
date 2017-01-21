private with Ada.Finalization;

generic
   type Content is limited private; -- Must have proper defaults
   type Ptr is access Content;
package Rx.Tools.Lazies is

--  Protected wrapper around a type that is created on first use
   type Lazy is tagged limited private;

   function Get (This : in out Lazy) return Ptr;

private

   protected type Safes (Parent : access Lazy) is
      procedure Get (X : in out Ptr);
      procedure Free;
   private
      Instance : Ptr;
   end Safes;

   use Ada.Finalization;

   type Lazy is new Limited_Controlled with record
      Safe : Safes (Lazy'Access);
   end record;

   overriding procedure Finalize (This : in out Lazy);

end Rx.Tools.Lazies;
