with Rx.Debug;
with Rx.Errors;
with Rx.Tools.Semaphores;

package body Rx.Op.Serialize is

   subtype Critical_Section is Tools.Semaphores.Critical_Section;

   type Serializer is new Operate.Operator with record
      Mutex : aliased Tools.Semaphores.Shared;
   end record;

   overriding procedure On_Next (This : in out Serializer; V : Operate.T);

   overriding procedure On_Complete  (This : in out Serializer);

   overriding procedure On_Error (This : in out Serializer; Error : Errors.Occurrence);

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This : in out Serializer; V : Operate.T) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      Debug.Trace ("serialize on_next");
      This.Get_Observer.On_Next (V);
   end On_Next;

   ------------------
   -- On_Complete  --
   ------------------

   overriding procedure On_Complete  (This : in out Serializer) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      Debug.Trace ("serialize on_complete");
      This.Get_Observer.On_Complete ;
   end On_Complete ;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error (This : in out Serializer; Error :        Errors.Occurrence) is
      CS : Critical_Section (This.Mutex'Access) with Unreferenced;
   begin
      Debug.Trace ("serialize on_error");
      This.Get_Observer.On_Error (Error);
   end On_Error;

   ------------
   -- Create --
   ------------

   function Create return Operate.Operator'Class is
   begin
      return Serializer'(Operate.Operator with
                           Mutex => Tools.Semaphores.Create_Reentrant);
   end Create;

end Rx.Op.Serialize;
