with Ada.Unchecked_Deallocation;

package body Rx.Lazy is

   procedure Free is new Ada.Unchecked_Deallocation (Content, Ptr);

   ---------
   -- Get --
   ---------

   function Get (This : in out Rx.Lazy.Lazy) return Ptr is
      X : Ptr;
   begin
      This.Safe.Get (X);
      return X;
   end Get;

   ----------
   -- Safe --
   ----------

   protected body Safe is

      ---------
      -- Get --
      ---------

      procedure Get (X : in out Ptr) is
      begin
         if Datum /= null then
            X := Datum;
         else
            Datum := new Content;
            X     := Datum;
         end if;
      end Get;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         Free (Datum);
      end Free;

   end Safe;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Rx.Lazy.Lazy) is
   begin
      This.Safe.Free;
   end Finalize;

end Rx.Lazy;
