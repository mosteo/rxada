with Ada.Unchecked_Deallocation;

package body Rx.Errors is

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Error : out Occurrence;
      From  :     Ada.Exceptions.Exception_Occurrence)
   is
   begin
      if Error.Instance = null then
         Error.Instance := new Ada.Exceptions.Exception_Occurrence;
      end if;
      Ada.Exceptions.Save_Occurrence (Error.Instance.all, From);
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
      Ada.Exceptions.Reraise_Occurrence (Error.Instance.all);
   end Reraise;

   procedure Free is new Ada.Unchecked_Deallocation (Ada.Exceptions.Exception_Occurrence, Except_Access);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize   (E : in out Occurrence) is
   begin
      Free (E.Instance);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust     (E : in out Occurrence) is
      Mine : Except_Access;
   begin
      if E.Instance /= null then
         Mine := new Ada.Exceptions.Exception_Occurrence;
         Ada.Exceptions.Save_Occurrence (Mine.all, E.Instance.all);
         Free (E.Instance);
         E.Instance := Mine;
      end if;
   end Adjust;

end Rx.Errors;
