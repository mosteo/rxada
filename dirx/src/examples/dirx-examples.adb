with Ada.Streams.Stream_IO;
with Ada.Text_IO;

package body DirX.Examples is

   ----------
   -- Hash --
   ----------

   function Hash (Filename : String) return GNAT.SHA512.Message_Digest is
      File   : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1048576);
      Last   : Ada.Streams.Stream_Element_Offset;
      Ctxt   : aliased GNAT.SHA512.Context;
      Hasher : GNAT.SHA512.Hash_Stream (Ctxt'Access);

      use Ada.Streams.Stream_IO;
   begin
      Open (File, Mode => In_File, Name => Filename);

      while not End_Of_File (File) loop
         Read (File, Buffer, Last);
         Hasher.Write (Buffer (1 .. Last));
      end loop;

      Close (File);

      return GNAT.SHA512.Digest (Ctxt);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (This : DirX.Directory_Entry) return Hashed_Entry is


      use Ada.Directories;

      Filename : constant String := Full_Name (This.Get_Entry);
   begin
      if Kind (Filename) = Directory then
         return Hashed_Entry'(Name_Len => Filename'Length,
                              Is_File  => False,
                              Name     => Filename);
      else
         return Hashed_Entry'(Name_Len => Filename'Length,
                              Is_File  => True,
                              Name     => Filename,
                              Hash     => Hash (Filename));
      end if;
   end Hash;

   ----------------
   -- Print_Hash --
   ----------------

   procedure Print_Hash (This : Hashed_Entry) is
      use Ada.Text_IO;
   begin
      if This.Is_File then
         Put_Line (This.Hash & "  " & This.Name);
      else
         Put_Line (GNAT.SHA512.Message_Digest'(others => ' ') & "  " & This.Name);
      end if;
   end Print_Hash;

end DirX.Examples;
