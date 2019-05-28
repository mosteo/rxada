with Ada.Directories;

with Rx.Std;

private with Rx.Tools.Shared_Data;

package DirX is

   -----------
   -- Types --
   -----------

   subtype Path is String;
   --  A Path can be either a folder, file, subpath, combination of those, etc.

   subtype Path_Observable is Rx.Std.Strings.Observable;

   type Name_Kinds is (Full_Name, Simple_Name);

   -----------------------
   -- Directory entries --
   -----------------------

   --  We need a nonlimited type to be able to use it with Rx, so this one
   --  encapsulates entries in Ada.Directories

   type Directory_Entry (<>) is tagged private;

   type Entry_Reference
     (The_Entry : access constant Ada.Directories.Directory_Entry_Type)
     is limited null record
     with Implicit_Dereference => The_Entry;

   function Get_Entry (This : Directory_Entry) return Entry_Reference;

   function Is_Directory (This : Directory_Entry) return Boolean;

private

   package AD renames Ada.Directories;

   use all type AD.File_Kind;

   type Entry_Access is access AD.Directory_Entry_Type;

   package Shared_Entries is new Rx.Tools.Shared_Data (AD.Directory_Entry_Type,
                                                       Entry_Access);

   type Directory_Entry is new Shared_Entries.Proxy with null record;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (This : Directory_Entry) return Entry_Reference is
     (Entry_Reference'(The_Entry => Shared_Entries.Proxy (This).Get.Actual));

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (This : Directory_Entry) return Boolean is
     (AD.Kind (This.Get_Entry) = AD.Directory);

end DirX;
