with Ada.Unchecked_Deallocation;

package body Rx.Errors is

   ------------
   -- Create --
   ------------

   function Create (From : Ada.Exceptions.Exception_Occurrence) return Occurrence is
   begin
      return E : Occurrence do
         Fill (E, From);
      end return;
   end Create;

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

   -------------
   -- Reraise --
   -------------

   procedure Reraise (Error : Occurrence) is
   begin
      Ada.Exceptions.Reraise_Occurrence (Error.Instance.all);
   end Reraise;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize   (E : in out Occurrence) is
      procedure Free is new Ada.Unchecked_Deallocation (Ada.Exceptions.Exception_Occurrence, Except_Access);
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
         E.Instance := Mine;
      end if;
   end Adjust;

end Rx.Errors;
