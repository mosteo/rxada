with Ada.Text_IO; use Ada.Text_IO;

with Rx;
with Rx.Count;
with Rx.From;
with Rx.Just;
with Rx.Map;
with Rx.Subscribe;
with Rx.Util;

procedure Rx.Examples.Basic is

   procedure Put_Line (I : Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;

   --  JUST
   package J1 is new Rx.Just (Integer, 1);
   package J2 is new Rx.Subscribe (J1.Output, Put_Line);

   --  FROM
   package F1 is new Rx.From (Integer, Rx.Util.IntegerArray, (3, 2, 1));
   package F2 is new Rx.Subscribe (F1.Output, Put_Line);

   --  COUNT
   package C1 is new Rx.Count (F1.Output);
   package C2 is new Rx.Subscribe (C1.Output, Put_Line);

   --  MAP
   package S1      is new Rx.Just (String, "Hello, World!");
   function Len (S : String) return Natural is (S'Length);
   package Str2Len is new Rx.Map (S1.Output, Integer, Len);
   package S3      is new Rx.Subscribe (Str2Len.Output, Put_Line);

begin
   null;
end Rx.Examples.Basic;
