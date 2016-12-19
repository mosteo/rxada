with Ada.Unchecked_Deallocation;

with Rx.Debug;
with Rx.Errors;
with Rx.Holders;
with Rx.Impl.Events;

package body Rx.Op.Debounce is

   package From renames Operate.From;
   package Into renames Operate.Into;

   package Events is new Rx.Impl.Events (Operate.Typed);
   --  package Shared is new Rx.Impl.Shared_Subscriber (Operate.Typed);

   task type Debouncer is

      entry Init (Window : Duration; Child : Into.Subscriber);

      entry On_Event (Event : Events.Event);

   end Debouncer;

   type Debouncer_Ptr is access all Debouncer;

   procedure Free_When_Terminated is new Ada.Unchecked_Deallocation (Debouncer, Debouncer_Ptr);

   type Operator is new Operate.Preserver with record
      Window : Duration;
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

      Self : Debouncer_Ptr := Debouncer'Unchecked_Access;

      Child     : Operate.Transform.Child_Holder;
      Window    : Duration;

      package Event_Holders is new Rx.Holders (Events.Event, "debounce_events");
      type Event_Holder is new Event_Holders.Definite with null record;

      Next	 : Event_Holder;
      Other      : Event_Holder;

      use all type Events.Kinds;

      -----------
      -- Flush --
      -----------

      procedure Flush (Elapsed : Boolean) is
         --  When Elapsed, the window has expired with nothing received
      begin

         if (Elapsed or else Other.Is_Valid) and then Next.Is_Valid then
            Child.Ref.On_Next (Events.Value (Next.CRef));
            Next.Clear;
         end if;

         if Other.Is_Valid then
            case Other.CRef.Kind is
               when On_Completed =>
                  Debug.Log ("Pre On_Completed", Debug.Warn);
                  Child.Ref.On_Completed;
                  Debug.Log ("Post On_Completed", Debug.Warn);
               when On_Error =>
                  declare
                     Error : Errors.Occurrence := Events.Error (Other.CRef);
                  begin
                     Child.Ref.On_Error (Error);
                  end;
               when Unsubscribe =>
                  Child.Ref.Unsubscribe;
               when On_Next =>
                  raise Program_Error with "Should never happen";
            end case;
         end if;

      end Flush;

   begin

      accept Init (Window : Duration; Child : Into.Subscriber) do
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
               delay Window;
               Flush (Elapsed => True);
            end select;
         end if;

         exit when Other.Is_Valid; -- Some other event

      end loop;

      Debug.Log ("FREEIN", Debug.Warn);
      Free_When_Terminated (Self);
      Debug.Log ("FREED", Debug.Warn);
   exception
      when E : others =>
         begin
            if Child.Ref.Is_Subscribed then
               Operate.Typed.Default_Error_Handler (Child.Ref, E);
            end if;
         exception
            when E : others =>
               Debug.Print (E);
         end;

         Free_When_Terminated (Self);
   end Debouncer;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        From.T;
                      Child : in out Into.Observer)
   is
      pragma Unreferenced (Child);
   begin
      This.Live.On_Event (Events.On_Next (V));
   end On_Next;

   overriding
   procedure On_Completed (This  : in out Operator;
                           Child : in out Into.Observer)
   is
      pragma Unreferenced (Child);
   begin
      This.Live.On_Event (Events.On_Completed);
   end On_Completed;

   overriding
   procedure On_Error (This  : in out Operator;
                       Error : in out Errors.Occurrence;
                       Child : in out Into.Observer)
   is
      pragma Unreferenced (Child);
   begin
      This.Live.On_Event (Events.On_Error (Error));
   end On_Error;

   ---------------
   -- Subscribe --
   ---------------

   overriding
   procedure Subscribe (Producer : in out Operator;
                        Consumer : in out Into.Subscriber)
   is
   begin
      Producer.Live  := new Debouncer;
      Producer.Live.Init (Producer.Window, Consumer);
      Operate.Preserver (Producer).Subscribe (Consumer);
      -- Consumer never to be used, so ideally we should use some always-failing consumer as child
   end Subscribe;

   -----------------
   -- Unsubscribe --
   -----------------

   overriding procedure Unsubscribe (This : in out Operator) is begin
      This.Live.On_Event (Events.Unsubscribe);
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
