private with Ada.Finalization;
-- with Ada.Containers.Indefinite_Holders;
-- with Ada.Containers.Indefinite_Doubly_Linked_Lists;
--  This is a workaround for a memory leak in the Indefinite_Holders (as of GPL2016)
--  It turns out Lists are broken too in instantiation from rx-from.adb
--  Rolling out my own holders (probably buggy too, or inneficient, or whatever...)

generic
   type Indef (<>) is private;
   Id : String := ""; -- Debug purposes only
package Rx.Holders is

   pragma Preelaborate;

   type Definite is tagged private;

   type Reference (Actual : access Indef) is limited private
   	with Implicit_Dereference => Actual;
   type Const_Ref (Actual : access constant Indef) is limited private
   	with Implicit_Dereference => Actual;

   function "+" (I : Indef)    return Definite with Inline;
   function "+" (D : Definite) return Indef    with Inline;

   procedure Hold (D : in out Definite; I : Indef);
   function Hold (I : Indef) return Definite renames "+";

   function Ref  (D : in out Definite) return Reference with Inline;
   function CRef (D :        Definite) return Const_Ref with Inline;

   function Is_Empty (D : Definite) return Boolean with Inline;

   procedure Clear (D : in out Definite);
   --  Dispose of the stored definite

private

   use Ada.Finalization;

   type Indef_Access is access Indef;
--     for Indef_Access'Storage_Pool use Debug.Debug_Pool;

   type Definite is new Ada.Finalization.Controlled with record
      Actual : Indef_Access;
   end record;

   overriding procedure Initialize (D : in out Definite);
   overriding procedure Adjust     (D : in out Definite);
   overriding procedure Finalize   (D : in out Definite);

   procedure Clear (D : in out Definite) renames Finalize;

   type Reference (Actual : access Indef) is limited null record;
   type Const_Ref (Actual : access constant Indef) is limited null record;

   function "+" (D : Definite) return Indef is (D.Actual.all);

   function Ref  (D : in out Definite) return Reference is (Actual => D.Actual);
   function CRef (D :        Definite) return Const_Ref is (Actual => D.Actual);

   function Is_Empty (D : Definite) return Boolean is (D.Actual = null);

end Rx.Holders;
