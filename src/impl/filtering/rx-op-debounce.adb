with Ada.Unchecked_Deallocation;

with Rx.Debug;
with Rx.Errors;
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

   type Debouncer_Ptr is access Debouncer;

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
      Child     : Operate.Transform.Child_Holder;
      Window    : Duration;

      Next	 : Events.Event (Events.On_Next);
      Next_Valid : Boolean := False;

      Other      : Events.Event (Events.On_Completed);
      Other_Valid: Boolean := False;

      use all type Events.Kinds;

      -----------
      -- Flush --
      -----------

      procedure Flush (Elapsed : Boolean) is
         --  When Elapsed, the window has expired with nothing received
      begin
         Debug.Log ("Flushing e:" & Elapsed'Img & " n:" & Next_Valid'Img & " o:" & Other_Valid'Img, Debug.Warn);

         if (Elapsed or else Other_Valid) and then Next_Valid then
            Child.Ref.On_Next (Events.Value (Next));
            Next_Valid := False;
         end if;

         if Other_Valid then
            case Other.Kind is
               when On_Completed =>
                  Child.Ref.On_Completed;
               when On_Error =>
                  declare
                     Error : Errors.Occurrence := Events.Error (Other);
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
         select
            accept On_Event (Event : Events.Event) do
               if Event.Kind = On_Next then
                  Next       := Event;
                  Next_Valid := True;
                  Debug.Log ("Got On_Next", Debug.Warn);
               else
                  Other       := Event;
                  Other_Valid := True;
                  Debug.Log ("Got Other", Debug.Warn);
               end if;
            end;
            Flush (Elapsed => False);
         or
            delay Window;
            Debug.Log ("Got ELAPSED", Debug.Warn);
            Flush (Elapsed => True);
         end select;

         exit when Other_Valid; -- Some other event

      end loop;
   exception
      when E : others =>
         if Child.Ref.Is_Subscribed then
            Operate.Typed.Default_Error_Handler (Child.Ref, E);
         end if;
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
      --procedure Free_When_Terminated is new Impl.Task_Deallocation (Debouncer, Debouncer_Ptr);
      procedure Free_When_Terminated is new Ada.Unchecked_Deallocation (Debouncer, Debouncer_Ptr);
   begin
      Producer.Live  := new Debouncer;
      Free_When_Terminated (Producer.Live);
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
