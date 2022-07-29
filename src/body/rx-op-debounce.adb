with Ada.Unchecked_Deallocation;

with Rx.Debug;
with Rx.Errors;
with Rx.Tools.Holders;
with Rx.Impl.Events;

package body Rx.Op.Debounce is

   package From renames Operate.From;
   package Into renames Operate.Into;

   package Events is new Rx.Impl.Events (Operate.Typed);
   --  package Shared is new Rx.Impl.Shared_Observer (Operate.Typed);

   task type Debouncer is

      entry Init (Window : Duration; Child : Into.Observer);

      entry On_Event (Event : Events.Event);

      entry Unsubscribe;

   end Debouncer;

   type Debouncer_Ptr is access all Debouncer;

   procedure Free_When_Terminated is new Ada.Unchecked_Deallocation (Debouncer, Debouncer_Ptr);

   type Operator is new Operate.Operator with record
      Window : Duration;
      Live   : Debouncer_Ptr;
      Done   : Boolean := False;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        From.T);
   --  Must always be provided

   overriding
   procedure On_Complete  (This  : in out Operator);
   --  By default calls Child.On_Complete

   overriding
   procedure On_Error (This  : in out Operator;
                       Error :        Errors.Occurrence);

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Observer);

   overriding
   procedure Unsubscribe (This : in out Operator);

   ---------------
   -- Debouncer --
   ---------------

   task body Debouncer is

      Self : Debouncer_Ptr := Debouncer'Unchecked_Access;

      Child     : Operate.Typed.Holders.Observer;
      Window    : Duration;

      package Event_Holders is new Rx.Tools.Holders (Events.Event, "debounce_events");
      type Event_Holder is new Event_Holders.Definite with null record;

      Next	 : Event_Holder;
      Other      : Event_Holder;

      Subscribed : Boolean := True;

      use all type Events.Kinds;

      -----------
      -- Flush --
      -----------

      procedure Flush (Elapsed : Boolean) is
         --  When Elapsed, the window has expired with nothing received
      begin

         if (Elapsed or else Other.Is_Valid) and then Next.Is_Valid then
            begin
               Child.Ref.On_Next (Events.Value (Next.CRef));
            exception
               when No_Longer_Subscribed =>
                  Debug.Log ("Debounce.Flush: Seen No_Longer_Subscribed", Debug.Note);
               when E : others =>
                  Operate.Typed.Defaults.Default_Error_Handler (Child.Ref, E);
            end;
            Next.Clear;
         end if;

         if Other.Is_Valid then
            case Other.CRef.Kind is
               when On_Complete  =>
                  Child.Ref.On_Complete ;
               when On_Error =>
                  Child.Ref.On_Error (Events.Error (Other.CRef));
               when On_Next =>
                  raise Program_Error with "Should never happen";
            end case;
         end if;

      end Flush;

   begin

      accept Init (Window : Duration; Child : Into.Observer) do
         Debouncer.Window := Window;
         Debouncer.Child.Hold (Child);
      end;

      loop
         if not Next.Is_Valid then
            select
               accept On_Event (Event : Events.Event) do
                  if Event.Kind = On_Next then
                     Next.Hold (Event);
                  else
                     Other.Hold (Event);
                     Flush (Elapsed => False);
                  end if;
               end;
            or
               accept Unsubscribe;
               Subscribed := False;
            or
               terminate;
            end select;
         else
            select
               accept On_Event (Event : Events.Event) do
                  if Event.Kind = On_Next then
                     Next.Hold (Event);
                  else
                     Other.Hold (Event);
                  end if;
               end;
               Flush (Elapsed => False);
            or
               accept Unsubscribe;
               Subscribed := False;
            or
               delay Window;
               Flush (Elapsed => True);
            end select;
         end if;

         exit when Other.Is_Valid or else not Subscribed; -- Some other event

      end loop;

      Free_When_Terminated (Self);
   exception
      when E : others =>
         Debug.Report (E, "At Debouncer final handler:", Debug.Warn, Reraise => False);
         Free_When_Terminated (Self);
   end Debouncer;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        From.T)
   is
   begin
      This.Live.On_Event (Events.On_Next (V));
   end On_Next;

   overriding
   procedure On_Complete  (This  : in out Operator)
   is
   begin
      This.Done := True;
      This.Live.On_Event (Events.On_Complete );
   end On_Complete ;

   overriding
   procedure On_Error (This  : in out Operator;
                       Error :        Errors.Occurrence)
   is
   begin
      This.Done := True;
      This.Live.On_Event (Events.On_Error (Error));
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Observer)
   is
   begin
      Producer.Live  := new Debouncer;
      Producer.Live.Init (Producer.Window, Consumer);
      Operate.Operator (Producer).Subscribe (Consumer);
      -- Consumer never to be used, so ideally we should use some always-failing consumer as child
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is begin
      if not This.Done then
         This.Done := True;
         This.Live.Unsubscribe;
      end if;
      Operate.Operator (This).Unsubscribe;
   end Unsubscribe;

   ------------
   -- Create --
   ------------

   function Create (Window : Duration) return Operate.Operator'Class is
   begin
      return Operator'(Operate.Operator with
                            Window => Window,
                            others => <>);
   end Create;

end Rx.Op.Debounce;
