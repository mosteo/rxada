with Rx.Indefinites;

package DirX.Observables is

   package RxEntries is new Rx.Indefinites (Directory_Entry);

   subtype Entry_Observable is RxEntries.Observable;

   function Directory_Entries
     (Directory : Path;
      Kind      : Name_Kinds := Full_Name;
      Pattern   : String := "*";
      Filter    : Ada.Directories.Filter_Type := (others => True))
      return      Entry_Observable;
   --  Same parameters as Ada.Directories.Start_Search

end DirX.Observables;
