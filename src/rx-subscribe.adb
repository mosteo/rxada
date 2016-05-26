with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package body Rx.Subscribe is

   --  SPEC

   type Observer is new Observable.Observer with null record;

   overriding
   procedure OnNext (This : in out Observer; V : Observable.T);

   overriding
   procedure OnCompleted (This : in out Observer);

   --  BODY

   overriding
   procedure OnNext (This : in out Observer; V : Observable.T) is
      pragma Unreferenced (This);
   begin
      OnNext (V);
   end OnNext;

   overriding
   procedure OnCompleted (This : in out Observer) is
      pragma Unreferenced (This);
   begin
      OnCompleted;
   end OnCompleted;

   O : aliased Observer;

begin
   Put_Line ("Subscribing: " & Ada.Tags.Expanded_Name (Observable.Instance'Tag));
   Observable.Instance.Subscribe (O'Access);
end Rx.Subscribe;
