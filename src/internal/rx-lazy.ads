private with Ada.Finalization;

generic
   type Content is limited private; -- Must have proper defaults
   type Ptr is access Content;
package Rx.Lazy is

--  Protected wrapper around a type that is created on first use
   type Lazy is tagged limited private;

   function Get (This : in out Rx.Lazy.Lazy) return Ptr;

private

   protected type Safe (Parent : access Rx.Lazy.Lazy) is
      procedure Get (X : in out Ptr);
      procedure Free;
   private
      Datum : Ptr;
   end Safe;

   use Ada.Finalization;

   type Lazy is new Limited_Controlled with record
      Safe : Rx.Lazy.Safe (Lazy'Access);
   end record;

   overriding procedure Finalize (This : in out Rx.Lazy.Lazy);

end Rx.Lazy;
