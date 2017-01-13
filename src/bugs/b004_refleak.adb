procedure B004_Refleak is

   package P is

      type Str_Holder is tagged private;
      -- IRL this would be a controlled type with proper allocation/deallocation of the held type

      function Create (S : String) return Str_Holder;

      type Reference (S : access String) is limited null record
        with Implicit_Dereference => S;

      function Ref (Str : Str_Holder) return Reference;

   private

      type Str_Ptr is access String;

      type Str_Holder is tagged record
         Ptr : Str_Ptr;
      end record;

      function Create (S : String) return Str_Holder is (Ptr => new String'(S));

      function Ref (Str : Str_Holder) return Reference is (Reference'(S => Str.Ptr));

   end P;

   S : constant P.Str_Holder := P.Create ("WTF");

   procedure Long_Lived is
   begin
      for I in 1 .. 9_999_999 loop
         exit when S.Ref = "XXX";
      end loop;
   end Long_Lived;

begin
   Long_Lived;
end B004_Refleak;
