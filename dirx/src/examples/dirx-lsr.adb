with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with DirX.Observables;

-- with Rx.Std; use Rx.Std;

procedure DirX.Lsr is
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
   --  Recursive listing of files
   DirX.Observables.RxEntries.Observables.For_Each
     (DirX.Observables.Directory_Entries (Target, Recursive => True),
      On_Next => Print_Full_Name'Unrestricted_Access);

end DirX.Lsr;
