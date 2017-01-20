package body Rx.Impl.Definite_Observers is

   ------------
   -- Create --
   ------------

   function Create (From : Contracts.Observer'Class) return Observer is
   begin
      return (Actual => Holders.Hold (From));
   end Create;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next
     (This : in out Observer;
      V : Contracts.T)
   is
   begin
      This.Actual.Ref.On_Next (V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Observer) is
   begin
      This.Actual.Ref.On_Complete ;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (This : in out Observer;
      Error : Errors.Occurrence)
   is
   begin
      This.Actual.Ref.On_Error (Error);
   end On_Error;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Observer) return Boolean is
   begin
      return This.Actual.Is_Valid;
   end Is_Valid;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Observer) is
   begin
      This.Actual.Clear;
   end Clear;

end Rx.Impl.Definite_Observers;
