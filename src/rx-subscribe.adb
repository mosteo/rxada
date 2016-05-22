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
   begin
      OnNext (V);
   end OnNext;

   overriding
   procedure OnCompleted (This : in out Observer) is
   begin
      OnCompleted;
   end OnCompleted;

   O : aliased Observer;

begin
   Put_Line ("Subscribing...");
   Observable.Instance.Subscribe (O'Access);
end Rx.Subscribe;
