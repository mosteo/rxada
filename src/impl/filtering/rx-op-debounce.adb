with Rx.Errors;
with Rx.Impl.Shared_Subscriber;
with Rx.Impl.Tasks;
package body Rx.Op.Debounce is

   package From renames Operate.From;
   package Into renames Operate.Into;

   package Shared is new Rx.Impl.Shared_Subscriber (Operate.Into);

   task type Debouncer is new Impl.Tasks.Transient with

      entry Init (Window : Duration; Child : Shared.Subscriber);

      entry On_Next (V : Operate.T);

      entry On_Completed;

      entry On_Error (E : Errors.Occurrence);

      entry Unsubscribe;

   end Debouncer;

   type Debouncer_Ptr is access Debouncer'Class;

   type Operator is new Operate.Preserver with record
      Window : Duration;
      Child  : Shared.Subscriber;
      Live   : Debouncer_Ptr;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        From.T;
                      Child : in out Into.Observer);
   --  Must always be provided

   overriding
   procedure On_Completed (This  : in out Operator;
                           Child : in out Into.Observer);
   --  By default calls Child.On_Complete

   overriding
   procedure On_Error (This  : in out Operator;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer);

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Subscriber);

   overriding
   procedure Unsubscribe (This : in out Operator);

   ---------------
   -- Debouncer --
   ---------------

   task body Debouncer is
      Reaper    : Impl.Tasks.Reaper (Debouncer'Unchecked_Access);

      Child     : Shared.Subscriber;
      Window    : Duration;

      Completed : Boolean := False;

      E         : Errors.Occurrence;
      Errored   : Boolean := False;

      V         : Operate.Typed.D;
      V_Stored  : Boolean := False;

      Unsubscribed : Boolean := False;

   begin
      accept Init (Window : Duration; Child : Shared.Subscriber) do
         Debouncer.Window := Window;
         Debouncer.Child  := Child;
      end;

      loop
         exit when Completed or else Errored or else Unsubscribed;

         select
            accept On_Next (V : Operate.T);
         or
            accept On_Completed;
         or
            accept On_Error (E : Errors.Occurrence);
         or
            accept Unsubscribe;
         or
            delay Window;
         end select;
      end loop;
   end Debouncer;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        From.T;
                      Child : in out Into.Observer) is
   begin
      null;
   end On_Next;

   overriding
   procedure On_Completed (This  : in out Operator;
                           Child : in out Into.Observer) is
   begin
      This.Live.On_Completed;
      Impl.Tasks.Reap_Now (Impl.Tasks.Transient_Ptr (This.Live));
   end On_Completed;

   overriding
   procedure On_Error (This  : in out Operator;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer)
   is
   begin
      This.Live.On_Error (Error);
      Impl.Tasks.Reap_Now (Impl.Tasks.Transient_Ptr (This.Live));
   end On_Error;

     ---------------
     -- Subscribe --
     ---------------

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Subscriber)
   is
   begin
      Producer.Child := Shared.Create (Consumer);
      Producer.Live  := new Debouncer;
      Producer.Live.Init (Producer.Window, Producer.Child);
      Operate.Preserver (Producer).Subscribe (Consumer);
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is begin
      This.Live.Unsubscribe;
      Impl.Tasks.Reap_Now (Impl.Tasks.Transient_Ptr (This.Live));
   end Unsubscribe;

   ------------
   -- Create --
   ------------

   function Create (Window : Duration) return Operate.Operator is
   begin
      return Op : Operator do
         Op.Window := Window;
      end return;
   end Create;

end Rx.Op.Debounce;
