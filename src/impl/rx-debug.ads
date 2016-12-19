with Ada.Exceptions;
with Ada.Tags;

with Gnat.IO;

package Rx.Debug is

   pragma Preelaborate;

   type Levels is (Note, -- Highly chatty
                   Info, -- Out-of-usual
                   Warn, -- Shouldn't happen but not critical (?)
                   Error  -- Something is definitely not working as expected
                   );

   Level : constant Levels := Info;
   --  Minimum level a message has to have for it to be printed

   procedure Log (S : String; Level : Levels); -- Prints S if above configured level

   procedure Put_Line (I : Rx_Integer);
   procedure Put_Line (S : String) renames Gnat.IO.Put_Line;

   function Image (I : Rx_Integer) return String is (Rx_Integer'Image (I));
   function Image (T : Ada.Tags.Tag) return String renames Ada.Tags.Expanded_Name;

   procedure Print (E : Ada.Exceptions.Exception_Occurrence);

end Rx.Debug;
