with Rx.Sources.Stateless;

package body Rx.Empty is

   type Void is null record;

   procedure On_Subscribe (State    : Void;
                           Observer : in out Typed.Observer) is null;

   package Empty_Sources is new Rx.Sources.Stateless (Typed, Void);

   -----------
   -- Empty --
   -----------

   function Empty return Typed.Observable is
   begin
      return Empty_Sources.Create (Void'(null record));
   end Empty;

   -----------
   -- Never --
   -----------

--     function Never return Typed.Observable is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Never unimplemented");
--        raise Program_Error with "Unimplemented function Never";
--        return Never;
--     end Never;
--
--     -----------
--     -- Error --
--     -----------
--
--     function Error
--       (E : Rx.Errors.Occurrence)
--        return Typed.Observable
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Error unimplemented");
--        raise Program_Error with "Unimplemented function Error";
--        return Error (E => E);
--     end Error;
--
--     -----------
--     -- Error --
--     -----------
--
--     function Error
--       (E : Ada.Exceptions.Exception_Occurrence)
--        return Typed.Observable
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Error unimplemented");
--        raise Program_Error with "Unimplemented function Error";
--        return Error (E => E);
--     end Error;

end Rx.Empty;
