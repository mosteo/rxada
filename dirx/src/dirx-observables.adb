with Rx.Errors;

package body DirX.Observables is

   package ADirs renames Ada.Directories;

   use RxEntries.Observables.Linkers;

   type Entry_Generator (Kind        : Name_Kinds;
                         Path_Len    : Natural;
                         Pattern_Len : Natural) is
     new RxEntries.Observables.Contracts.Observable with
      record
         Path    : String (1 .. Path_Len);
         Pattern : String (1 .. Pattern_Len);
         Filter  : AD.Filter_Type;
      end record;

   overriding procedure Subscribe
     (Producer : in out Entry_Generator;
      Consumer : in out RxEntries.Observables.Contracts.Observer'Class);


   -----------------------
   -- Directory_Entries --
   -----------------------

   function Directory_Entries
     (Directory : Path;
      Kind      : Name_Kinds := Full_Name;
      Pattern   : String := "*";
      Filter    : Ada.Directories.Filter_Type := (others => True))
      return      Entry_Observable
   is
     (Entry_Generator'(Kind        => Kind,
                       Path_Len    => Directory'Length,
                       Pattern_Len => Pattern'Length,
                       Path        => Directory,
                       Pattern     => Pattern,
                       Filter      => Filter));

   ---------------------------------
   -- Directory_Entries_Recursive --
   ---------------------------------

   function Directory_Entries_Recursive
     (Directory : Path;
      Kind      : Name_Kinds := Full_Name;
      Pattern   : String := "*";
      Filter    : Ada.Directories.Filter_Type := (others => True))
      return      Entry_Observable
   is
      use all type ADirs.Filter_Type;

      --  Include dirs even if user doesn't want them
      Filter_With_Dirs : constant ADirs.Filter_Type :=
                           Filter or
                           ADirs.Filter_Type'(ADirs.Directory => True,
                                              others          => False);
      use RxEntries.Actions;
   begin
      return Directory_Entries (Directory => Directory,
                                Kind      => Kind,
                                Pattern   => Pattern,
                                Filter    => Filter_With_Dirs)
        --  Final removal of directories, if user didn't ask for them
        & (if not Filter (AD.Directory)
           then RxEntries.Observables.Filter (not Is_Directory'Access)
           else RxEntries.Observables.No_Op);
   end Directory_Entries_Recursive;

   --------------------
   -- Observe_Common --
   --------------------

   function Observe_Common (This    : Directory_Entry;
                            Recurse : Boolean) return Entry_Observable is
     (RxEntries.Observables.Just (This)
      & RxEntries.Observables.Merge_With
        (if AD.Kind (This.Get_Entry) = AD.Directory
         then (if Recurse
               then Directory_Entries_Recursive (AD.Full_Name (This.Get_Entry))
               else Directory_Entries (AD.Full_Name (This.Get_Entry)))
         else RxEntries.Observables.Empty));

   ---------------
   -- Subscribe --
   ---------------

   overriding procedure Subscribe
     (Producer : in out Entry_Generator;
      Consumer : in out RxEntries.Observables.Contracts.Observer'Class)
   is
      Search : AD.Search_Type;
   begin
      AD.Start_Search (Search    => Search,
                       Directory => Producer.Path,
                       Pattern   => Producer.Pattern,
                       Filter    => Producer.Filter);

      while AD.More_Entries (Search) loop
         declare
            DE   : constant Entry_Access    := new AD.Directory_Entry_Type;
            Item : constant Directory_Entry := Wrap (DE);
         begin
            AD.Get_Next_Entry (Search, DE.all);
            Consumer.On_Next (Item);
         end;
      end loop;

      AD.End_Search (Search);
      Consumer.On_Complete;
   exception
      when E : others =>
         AD.End_Search (Search);
         Consumer.On_Error (Rx.Errors.Create (E));
   end Subscribe;

end DirX.Observables;
