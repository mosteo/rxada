with Rx.Debug;
with Rx.Errors;

package body DirX.Observables is

   use RxEntries.Observables.Linkers;

   --  Entry_Generator type is the observable that will enumerate dir contents
   --    upon subscription

   type Entry_Generator (Path_Len : Natural) is
     new RxEntries.Observables.Contracts.Observable with
      record
         Path    : String (1 .. Path_Len);
      end record;

   overriding procedure Subscribe
     (Producer : in out Entry_Generator;
      Consumer : in out RxEntries.Observables.Contracts.Observer'Class);

   -----------------------
   -- Directory_Entries --
   -----------------------

   function Directory_Entries
     (Directory : Path;
      Recursive : Boolean)
      return      Entry_Observable
   is
     (Entry_Generator'(Path_Len => Directory'Length,
                       Path     => Directory)
      & (if Recursive
         then RxEntries.Observables.Expand (Observe'Access)
         else RxEntries.Observables.No_Op));

   -------------
   -- Observe --
   -------------

   function Observe (This : Directory_Entry) return Entry_Observable is
     (if This.Is_Directory and then
         AD.Simple_Name (This.Get_Entry) /= "." and then
         AD.Simple_Name (This.Get_Entry) /= ".."
      then Directory_Entries (AD.Full_Name (This.Get_Entry),
                              Recursive => False)
      else RxEntries.Observables.Empty);

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
                       Pattern   => "*",
                       Directory => Producer.Path);

      while AD.More_Entries (Search) loop
         declare
            DE   : constant Entry_Access    := new AD.Directory_Entry_Type;
            Item : constant Directory_Entry := Wrap (DE);
         begin
            AD.Get_Next_Entry (Search, DE.all);
            Rx.Debug.Trace ("dir_entry on_next");
            Consumer.On_Next (Item);
         end;
      end loop;

      AD.End_Search (Search);
      Rx.Debug.Trace ("dir_entry on_complete");
      Consumer.On_Complete;
   exception
      when E : others =>
         AD.End_Search (Search);
         Rx.Debug.Trace ("dir_entry on_error");
         Consumer.On_Error (Rx.Errors.Create (E));
   end Subscribe;

end DirX.Observables;
