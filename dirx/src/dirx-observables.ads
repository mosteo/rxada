with Rx.Indefinites;

package DirX.Observables is

   package RxEntries is new Rx.Indefinites (DirX.Directory_Entry);

   subtype Entry_Observable is RxEntries.Observable;

   function Directory_Entries
     (Directory : Path;
      Kind      : Name_Kinds := Full_Name;
      Pattern   : String := "*";
      Filter    : Ada.Directories.Filter_Type := (others => True))
      return      Entry_Observable;
   --  Same parameters as Ada.Directories.Start_Search

   function Directory_Entries_Recursive
     (Directory : Path;
      Kind      : Name_Kinds := Full_Name;
      Pattern   : String := "*";
      Filter    : Ada.Directories.Filter_Type := (others => True))
      return      Entry_Observable;

   function Observe (This : Directory_Entry) return Entry_Observable;
   --  For use in Flat_Map: turns an entry into an observable.
   --  If entry is Directory, it emits itself and all its *immediate* children
   --  Otherwise it is Just (This)

   function Observe_Recursive (This : Directory_Entry) return Entry_Observable;
   --  As Observe, but recursive

private

   function Observe_Common (This    : Directory_Entry;
                            Recurse : Boolean) return Entry_Observable;

   function Observe (This : Directory_Entry) return Entry_Observable is
      (Observe_Common (This, Recurse => False));

   function Observe_Recursive (This : Directory_Entry) return Entry_Observable
     is (Observe_Common (This, Recurse => True));

end DirX.Observables;
