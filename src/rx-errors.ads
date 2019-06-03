with Ada.Exceptions;
with Ada.Finalization;

package Rx.Errors is

   pragma Preelaborate;

   type Occurrence is tagged private;

   procedure Fill (Error : out Occurrence;
                   From  :     Ada.Exceptions.Exception_Occurrence);

   function Create (From : Ada.Exceptions.Exception_Occurrence) return Occurrence;

   procedure Reraise (Error : Occurrence);

   function Get_Exception (Error : Occurrence)
                           return access constant Ada.Exceptions.Exception_Occurrence;

private

   type Except_Access is access Ada.Exceptions.Exception_Occurrence;

   type Occurrence is new Ada.Finalization.Controlled with record
      Instance : Except_Access;
   end record;

   overriding procedure Finalize   (E : in out Occurrence);
   overriding procedure Adjust     (E : in out Occurrence);

   function Get_Exception (Error : Occurrence)
                           return access constant Ada.Exceptions.Exception_Occurrence
     is (Error.Instance);

end Rx.Errors;
