with Ada.Exceptions;
with Ada.Tags;

with Gnat.IO;

private with Ada.Strings;
private with Ada.Strings.Fixed;

package Rx.Debug is

   pragma Preelaborate;

   type Levels is (Impl, -- Implementation detail, for debugging
                   Note, -- Highly chatty
                   Info, -- Out-of-usual
                   Warn, -- Shouldn't happen but not critical (?)
                   Error  -- Something is definitely not working as expected
                   );

   Level : Levels := Info;
   --  Minimum level a message has to have for it to be printed

   procedure Log (S : String; Level : Levels); -- Prints S if above configured level

   procedure Trace (S : String); -- Log at Impl level

   procedure Put_Line (I : Rx_Integer);
   procedure Put_Line (S : String) renames Gnat.IO.Put_Line;

   function Trim (S : String) return String;

   function Image (I : Rx_Integer) return String is (Rx_Integer'Image (I));
   function Image (T : Ada.Tags.Tag) return String renames Ada.Tags.Expanded_Name;

   procedure Print (E : Ada.Exceptions.Exception_Occurrence);

   procedure Report (E       : Ada.Exceptions.Exception_Occurrence;
                     Msg     : String;
                     Level   : Levels := Error;
                     Reraise : Boolean := False);
   --  Prints Msg at level Level, prints the exception and optionally re-raises

private

   function Trim (S : String) return String is (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));

end Rx.Debug;
