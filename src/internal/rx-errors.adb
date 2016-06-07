package body Rx.Errors is

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Error : out Occurrence;
      From  :     Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Save_Occurrence (Error.Instance, From);
   end Fill;

   -----------------
   -- Set_Handled --
   -----------------

   procedure Set_Handled
     (Error : in out Occurrence;
      Dealt_With : Boolean := True)
   is
   begin
      Error.Handled := Dealt_With;
   end Set_Handled;

   ----------------
   -- Is_Handled --
   ----------------

   function Is_Handled (Error : Occurrence) return Boolean is (Error.Handled);

   -------------
   -- Reraise --
   -------------

   procedure Reraise (Error : Occurrence) is
   begin
      Ada.Exceptions.Reraise_Occurrence (Error.Instance);
   end Reraise;

end Rx.Errors;
