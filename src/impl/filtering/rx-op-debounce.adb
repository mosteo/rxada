with Rx.Errors;
with Rx.Impl.Shared_Subscriber;

package body Rx.Op.Debounce is

   package Shared is new Rx.Impl.Shared_Subscriber (Operate.Into);

   task type Debouncer is

      entry Init (Window : Duration; Child : Shared.Subscriber);

      entry On_Next (V : Operate.T);

      entry On_Completed;

      entry On_Error (E : Errors.Occurrence);

   end Debouncer;

   task body Debouncer is
      Child     : Shared.Subscriber;
      Window    : Duration;
      Completed : Boolean := False;
      V    	: Operate.Typed.D;
   begin
      accept Init (Window : Duration; Child : Shared.Subscriber) do
         Debouncer.Window := Window;
         Debouncer.Child  := Child;
      end;

      loop
         select
            accept On_Next (V : Operate.T);
         or
            accept On_Completed;
         or
            accept On_Error (E : Errors.Occurrence);
         or
            terminate;
         end select;
      end loop;
   end Debouncer;

   ------------
   -- Create --
   ------------

   function Create (Window : Duration) return Operate.Operator is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      raise Program_Error with "Unimplemented function Create";
      return Create (Window => Window);
   end Create;

end Rx.Op.Debounce;
