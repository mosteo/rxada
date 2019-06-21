with Rx.Indefinites;

package DirX.Observables is

   package RxEntries is new Rx.Indefinites (DirX.Directory_Entry);

   subtype Entry_Observable is RxEntries.Observable;

   function Directory_Entries
     (Directory : Path;
      Recursive : Boolean)
      return      Entry_Observable;
   --  Enumerate entries in a given path.
   --  Optionally, enter into found directories and emit their contents too

   function Observe (This : Directory_Entry) return Entry_Observable;
   --  For use in Flat_Map: turns a directory entry into an observable.
   --  If entry is Directory, it emits all its *immediate* children
   --  Otherwise, nothing is emitted

   --  TODO: filters for file kind, file pattern, but do it in DirX root

end DirX.Observables;
