with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;

package Rx is

   --  pragma Preelaborate;

   generic
      type T (<>) is private;
   package Base is

      type Observable is limited interface;

      --  The Observable provided by each package instance
      Instance : access Observable'Class;

      type Observer is limited interface;

      procedure OnNext      (This : in out Observer; V : T) is abstract;
      procedure OnCompleted (This : in out Observer) is null;
      procedure Subscribe   (O : in out Observable;
                             S : access Observer'Class) is abstract;

      --  Helper types for subscription management

      type    Observer_Access  is access all Observer'Class;
      package Observer_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Observer_Access);

      --  Helpers for indefinite values

      package  Holder is new Ada.Containers.Indefinite_Holders (T);
      type     TH     is new Holder.Holder with null record;
      function Hold (V : T) return TH renames To_Holder;

   end Base;

   --  Helpers
   type IntegerArray is array (Integer range <>) of Integer;

   procedure Put_Line (I : Integer);

   procedure Debug (S : String);

end Rx;
