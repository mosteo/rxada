with Rx.Errors;

package body Rx.Op.Buffer is

   use Transform.Into.Conversions;

   type Counter is new Transform.Transformer with record
      Container : Transform.Into.D := Empty;

      Need      : Positive;
      Have      : Natural := 0;

      Skip      : Natural := 0;
      Skipped   : Natural := 0;

      Skipping  : Boolean := False;
   end record;

   overriding procedure On_Next (This  : in out Counter;
                                 V     :        Transform.From.T;
                                 Child : in out Transform.Into.Observer'Class);

   overriding procedure On_Completed (This  : in out Counter;
                                      Child : in out Transform.Into.Observer'Class);

   overriding procedure On_Error (This  : in out Counter;
                                  Error : in out Errors.Occurrence;
                                  Child : in out Transform.Into.Observer'Class);

   procedure Emit (This : in out Counter; Child : in out Transform.Into.Observer'Class) is
   begin
      This.Have := 0;
      Child.On_Next (+ This.Container);
      This.Container := Empty;
   end Emit;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Counter;
                                 V     :        Transform.From.T;
                                 Child : in out Transform.Into.Observer'Class) is
   begin
      if This.Skipping then
         This.Skipped := This.Skipped + 1;

         if This.Skipped = This.Skip then
            This.Skipping := False;
         end if;
      else
         Append (This.Container, V);
         This.Have := This.Have + 1;

         if This.Have = This.Need then
            Emit (This, Child);

            if This.Skip > 0 then
               This.Skipping := True;
               This.Skipped  := 0;
            end if;
         end if;
      end if;
   end On_Next;

   ------------------
   -- On_Completed --
   ------------------

   overriding procedure On_Completed (This  : in out Counter;
                                      Child : in out Transform.Into.Observer'Class) is
   begin
      if This.Have > 0 then
         Emit (This, Child);
      end if;
      Child.On_Completed;
   end On_Completed;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This  : in out Counter;
                                  Error : in out Errors.Occurrence;
                                  Child : in out Transform.Into.Observer'Class) is
   begin
      if This.Have > 0 then
         Emit (This, Child);
         Child.On_Error (Error);
      end if;
   end On_Error;

   ------------
   -- Create --
   ------------

   function Create
     (Every : Positive;
      Skip : Natural := 0)
      return Transform.Transformer'Class
   is
   begin
      return Counter'(Transform.Transformer with
                      Need   => Every,
                      Skip   => Skip,
                      others => <>);
   end Create;

end Rx.Op.Buffer;
