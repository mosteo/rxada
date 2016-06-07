with Ada.Exceptions;

package Rx.Errors is

   pragma Preelaborate;

   type Occurrence is tagged limited private;

   procedure Fill (Error : out Occurrence;
                   From  :     Ada.Exceptions.Exception_Occurrence);

   procedure Set_Handled (Error : in out Occurrence; Dealt_With : Boolean := True);

   function Is_Handled (Error : Occurrence) return Boolean;

   procedure Reraise (Error : Occurrence);

private

   type Occurrence is tagged limited record
      Instance : Ada.Exceptions.Exception_Occurrence;
      Handled  : Boolean := False;
   end record;

end Rx.Errors;
