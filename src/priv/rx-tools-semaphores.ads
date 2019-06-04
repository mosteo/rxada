private with Ada.Finalization;
private with Ada.Task_Identification;

private with Rx.Tools.Shared_Data;

private with System.Address_Image;

package Rx.Tools.Semaphores is

   type Shared is private;
   --  A ref-counted semaphore which is initially invalid

   function Create_Reentrant (Fake : Boolean := False) return Shared;
   --  Allocate an available semaphore (or a fake one that does nothing)

   type Critical_Section (Mutex : access Shared) is tagged limited private;
   --  Declare an instance of this type in the scope to be made exclusive
   --  It automatically seizes/releases the semaphore on entering/exiting the scope of declaration
   --  The mutex is copied and could be disposed of by the caller inside the critical section

   function Image (This : Shared) return String;

   function Image (This : Critical_Section) return String is
     (Image (This.Mutex.all));

private

   protected type Reentrant is
      entry Seize;
      procedure Release;
   private
      entry Wait;
      Count : Natural := 0;
      Owner  :Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
   end Reentrant;

   type Reentrant_Ptr is access Reentrant;

   package Shared_Semaphores is new Rx.Tools.Shared_Data (Reentrant, Reentrant_Ptr);

   type Shared is new Shared_Semaphores.Proxy with record
      Fake : Boolean := False;
   end record;

   not overriding procedure Seize (This : in out Shared);

   not overriding procedure Release (This : in out Shared);

   overriding function Wrap (I : not null Reentrant_Ptr) return Shared is
      (Shared_Semaphores.Wrap (I) with Fake => False);

   function Create_Reentrant (Fake : Boolean := False) return Shared is
     (if Fake then
        (Shared_Semaphores.Proxy with Fake => True)
      else
        (Wrap (new Reentrant)));

   function Image (This : Shared) return String is
      ("#" & System.Address_Image (This.Get.Actual.all'Address));

   type Critical_Section (Mutex : not null access Shared) is new Ada.Finalization.Limited_Controlled
   with record
      Sem : Shared;
   end record;

   overriding procedure Initialize (This : in out Critical_Section);
   overriding procedure Finalize   (This : in out Critical_Section);

end Rx.Tools.Semaphores;
