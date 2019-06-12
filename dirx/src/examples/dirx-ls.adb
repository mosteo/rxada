with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with DirX.Observables;

-- with Rx.Std; use Rx.Std;

procedure DirX.Ls is
   use Ada.Command_Line;

   ---------------------
   -- Print_Full_Name --
   ---------------------

   procedure Print_Full_Name (This : DirX.Directory_Entry) is
   begin
      Ada.Text_IO.Put_Line (Ada.Directories.Full_Name (This.Get_Entry));
   end Print_Full_Name;

   Target : constant Path := (if Argument_Count = 0
                              then "."
                              else Argument (1));
begin
   --  Ordinary listing of entries in given folder
   DirX.Observables.RxEntries.Observables.For_Each
     (DirX.Observables.Directory_Entries (Target, Recursive => False),
      On_Next => Print_Full_Name'Unrestricted_Access);

end DirX.Ls;
