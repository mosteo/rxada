with Rx.Errors;

package body DirX.Observables is

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
   begin
      return Entry_Generator'(Kind        => Kind,
                              Path_Len    => Directory'Length,
                              Pattern_Len => Pattern'Length,
                              Path        => Directory,
                              Pattern     => Pattern,
                              Filter      => Filter);
   end Directory_Entries;

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
