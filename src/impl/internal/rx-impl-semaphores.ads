private with Ada.Finalization;

with GNAT.Semaphores;

private with System;

package Rx.Impl.Semaphores is

   type Binary is limited private;
   -- The semaphore type to be shared across threads

   type Critical_Section (Mutex : access Binary) is limited private;
   --  Declare an instance of this type in the scope to be made exclusive
   --  It automatically seizes/releases the semaphore on entering/exiting the scope of declaration

private

   type Binary is limited record
      Sem : GNAT.Semaphores.Binary_Semaphore (Initially_Available => True,
                                              Ceiling             => System.Default_Priority);
   end record;

   type Critical_Section (Mutex : access Binary) is new Ada.Finalization.Limited_Controlled with
     null record;

   overriding procedure Initialize (This : in out Critical_Section);
   overriding procedure Finalize   (This : in out Critical_Section);

end Rx.Impl.Semaphores;
