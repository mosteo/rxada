with Ada.Unchecked_Deallocation;

package body Rx.Tools.Lazies is

   procedure Free is new Ada.Unchecked_Deallocation (Content, Ptr);

   ---------
   -- Get --
   ---------

   function Get (This : in out Lazy) return Ptr is
      X : Ptr;
   begin
      This.Safe.Get (X);
      return X;
   end Get;

   ----------
   -- Safe --
   ----------

   protected body Safes is

      ---------
      -- Get --
      ---------

      procedure Get (X : in out Ptr) is
      begin
         if Instance /= null then
            X := Instance;
         else
            Instance := new Content;
            X     := Instance;
         end if;
      end Get;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         Free (Instance);
      end Free;

   end Safes;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Lazy) is
   begin
      This.Safe.Free;
   end Finalize;

end Rx.Tools.Lazies;
