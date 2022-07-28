with Ada.Calendar; use Ada.Calendar;

with Rx.Debug;
with Rx.Errors;

package body Rx.Op.Stopwatch is

   type Operator is new Preserver.Operator with record
      Start, Previous : Time;
      Inspect         : Actions.Inspector;
   end record;

   procedure Call (This : in out Operator'Class; Kind : Rx_Event_Kinds);
   --  Do measurements and call

   overriding procedure On_Next (This : in out Operator; V : Preserver.T);

   overriding procedure On_Complete  (This : in out Operator);

   overriding procedure On_Error (This  : in out Operator;
                                  Error :        Errors.Occurrence);

   overriding procedure Subscribe (This     : in out Operator;
                                   Consumer : in out Preserver.Observer'Class);


   ----------
   -- Call --
   ----------

   procedure Call (This : in out Operator'Class; Kind : Rx_Event_Kinds) is
      Now : constant Time := Clock;
   begin
      This.Inspect (Kind,
                    Since_Previous     => Now - This.Previous,
                    Since_Subscription => Now - This.Start);
      This.Previous := Now;
   end Call;

   ------------
   -- Create --
   ------------

   function Create (Callback : not null Actions.Inspector)
                    return Preserver.Operator'Class is
   begin
      return Operator'(Preserver.Operator with
                       Inspect => Callback,
                       Start   => <>,
                       Previous => <>);
   end Create;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Operator) is
   begin
      Debug.Trace ("stopwatch on_complete");
      Call (This, On_Complete);
      This.Get_Observer.On_Complete ;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Operator; Error :        Errors.Occurrence) is
   begin
      Debug.Trace ("stopwatch on_error");
      Call (This, On_Error);
      This.Get_Observer.On_Error (Error);
   end On_Error;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Operator; V : Preserver.T) is
   begin
      Debug.Trace ("stopwatch on_next");
      Call (This, On_Next);
      This.Get_Observer.On_Next (V);
   end On_Next;

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe (This     : in out Operator;
                                   Consumer : in out Preserver.Observer'Class)
   is
   begin
      This.Start    := Clock;
      This.Previous := This.Start;
      Preserver.Operator (This).Subscribe (Consumer);
   end Subscribe;

end Rx.Op.Stopwatch;
