with Rx.Errors;
with Rx.Impl.Events;
with Rx.Impl.Shared_Subscriber;
with Rx.Impl.Task_Deallocation;

package body Rx.Op.Debounce is

   package From renames Operate.From;
   package Into renames Operate.Into;

   package Events is new Rx.Impl.Events (Operate.Typed);
   package Shared is new Rx.Impl.Shared_Subscriber (Operate.Typed);

   task type Debouncer is

      entry Init (Window : Duration; Child : Shared.Subscriber);

      entry On_Event (Event : Events.Event);

   end Debouncer;

   type Debouncer_Ptr is access Debouncer;

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
      Child     : Shared.Subscriber;
      Window    : Duration;

      Next	 : Events.Event (Events.On_Next);
      Next_Valid : Boolean := False;

      Other      : Events.Event (Events.On_Completed);
      Other_Valid: Boolean := False;

      use all type Events.Kinds;

      procedure Flush (Elapsed : Boolean) is
         --  When Elapsed, the window has expired with nothing received
      begin
         if (Elapsed or else Other_Valid) and then Next_Valid then
            Child.On_Next (Events.Value (Next));
            Next_Valid := False;
         end if;

         if Other_Valid then
            case Other.Kind is
               when On_Completed =>
                  Child.On_Completed;
               when On_Error =>
                  declare
                     Error : Errors.Occurrence := Events.Error (Other);
                  begin
                     Child.On_Error (Error);
                  end;
               when Unsubscribe =>
                  Child.Unsubscribe;
               when On_Next =>
                  raise Program_Error with "Should never happen";
            end case;
         end if;

      end Flush;
   begin

      accept Init (Window : Duration; Child : Shared.Subscriber) do
         Debouncer.Window := Window;
         Debouncer.Child  := Child;
      end;

      loop
         select
            accept On_Event (Event : Events.Event) do
               if Event.Kind = On_Next then
                  Next       := Event;
                  Next_Valid := True;
               else
                  Other       := Event;
                  Other_Valid := True;
               end if;
            end;
            Flush (Elapsed => False);
         or
            delay Window;
            Flush (Elapsed => True);
         end select;

         exit when Other_Valid; -- Some other event

      end loop;
   exception
      when E : others =>
         if Child.Is_Subscribed then
            Operate.Typed.Default_Error_Handler (Child, E);
         end if;
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
   end On_Completed;

   overriding
   procedure On_Error (This  : in out Operator;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer)
   is
   begin
      This.Live.On_Error (Error);
   end On_Error;

     ---------------
     -- Subscribe --
     ---------------

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Subscriber)
   is
      procedure Free_When_Terminated is new Impl.Task_Deallocation (Debouncer, Debouncer_Ptr);
   begin
      Producer.Child := Shared.Create (Consumer);
      Producer.Live  := new Debouncer;
      Free_When_Terminated (Producer.Live);
      Producer.Live.Init (Producer.Window, Producer.Child);
      Operate.Preserver (Producer).Subscribe (Consumer);
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is begin
      This.Live.Unsubscribe;
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
