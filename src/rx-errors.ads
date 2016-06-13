with Ada.Exceptions;
with Ada.Finalization;

package Rx.Errors is

   pragma Preelaborate;

   type Occurrence is new Ada.Finalization.Controlled with private;

   procedure Fill (Error : out Occurrence;
                   From  :     Ada.Exceptions.Exception_Occurrence);

   procedure Set_Handled (Error : in out Occurrence; Dealt_With : Boolean := True);

   function Is_Handled (Error : Occurrence) return Boolean;

   procedure Reraise (Error : Occurrence);

   function Get_Exception (Error : Occurrence) return access constant Ada.Exceptions.Exception_Occurrence;

private

   type Except_Access is access Ada.Exceptions.Exception_Occurrence;

   type Occurrence is new Ada.Finalization.Controlled with record
      Instance : Except_Access;
      Handled  : Boolean := False;
   end record;

   overriding procedure Finalize   (E : in out Occurrence);
   overriding procedure Adjust     (E : in out Occurrence);

   function Get_Exception (Error : Occurrence) return access constant Ada.Exceptions.Exception_Occurrence
     is (Error.Instance);

end Rx.Errors;
