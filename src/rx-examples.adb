with Rx;
with Rx.Count;
with Rx.From;
with Rx.Just;
with Rx.Map;
with Rx.Subscribe;

procedure Rx.Examples is

   --  JUST
   package Just is new Rx.Just (Integer, 1);
   package JustSubscribe is new Rx.Subscribe (Just.Output, Rx.Writeln);

   --  FROM
   package From is new Rx.From (Integer, Rx.IntegerArray, (3, 2, 1));
   package FromSubscribe is new Rx.Subscribe (From.Output, Writeln);

   --  COUNT
   package Count is new Rx.Count (From.Output);
   package CountSubscribe is new Rx.Subscribe (Count.Output, Writeln);

   --  MAP
   package JustStr is new Rx.Just (String, "Hello, World!");
   function Len (S : String) return Natural is (S'Length);
   package Str2Len is new Rx.Map (JustStr.Output, Integer, Len);
   package StrSubscribe is new Rx.Subscribe (Str2Len.Output, Writeln);

begin
   null;
end Rx.Examples;
