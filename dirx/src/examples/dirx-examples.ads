with GNAT.SHA512;

with DirX.Observables;

with Rx.Indefinites;
with Rx.Operators;

package DirX.Examples is

   --  Types to store a hash with a filename and use it with Rx

   type Hashed_Entry (Name_Len : Positive; Is_File : Boolean) is record
      Name : String (1 .. Name_Len);

      case Is_File is
         when True  => Hash : GNAT.SHA512.Message_Digest;
         when False => null;
      end case;
   end record;

   package RxHashed is new Rx.Indefinites (Hashed_Entry);

   package Entry_To_Hash is new Rx.Operators
     (DirX.Observables.RxEntries.Observables,
      RxHashed.Observables);

   -----------------
   -- Subprograms --
   -----------------

   function Hash (This : DirX.Directory_Entry) return Hashed_Entry;

   procedure Print_Hash (This : Hashed_Entry);

end DirX.Examples;
