private with Ada.Finalization;

private with GNAT.Semaphores;

private with Rx.Impl.Shared_Data;

private with System;

package Rx.Impl.Semaphores is

   type Shared_Binary is private;
   --  A ref-counted semaphore which is initially invalid

   function Create (Fake : Boolean := False) return Shared_Binary;
   --  Allocate a valid semaphore (or a fake one that does nothing)

   type Critical_Section (Mutex : access Shared_Binary) is limited private;
   --  Declare an instance of this type in the scope to be made exclusive
   --  It automatically seizes/releases the semaphore on entering/exiting the scope of declaration

private

   subtype Binary is GNAT.Semaphores.Binary_Semaphore;

   type Binary_Ptr is access Binary;

   package Shared_Semaphores is new Rx.Impl.Shared_Data (Binary, Binary_Ptr);

   type Shared_Binary is new Shared_Semaphores.Proxy with record
      Fake : Boolean := False;
   end record;

   not overriding procedure Seize (This : in out Shared_Binary);

   not overriding procedure Release (This : in out Shared_Binary);

   overriding function Wrap (I : not null Binary_Ptr) return Shared_Binary is
      (Shared_Semaphores.Wrap (I) with Fake => False);

   function Create (Fake : Boolean := False) return Shared_Binary is
     (if Fake then
        (Shared_Semaphores.Proxy with Fake => True)
      else
        (Wrap (new Binary (Initially_Available => True,
                           Ceiling             => System.Default_Priority))));

   type Critical_Section (Mutex : not null access Shared_Binary) is new Ada.Finalization.Limited_Controlled
   with record
      Sem : Shared_Binary;
   end record;

   overriding procedure Initialize (This : in out Critical_Section);
   overriding procedure Finalize   (This : in out Critical_Section);

end Rx.Impl.Semaphores;
