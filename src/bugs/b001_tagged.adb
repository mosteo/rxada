procedure B001_Tagged is

   generic
      type X is private;
   package Untagged is

      type Y is new X;

   end Untagged;

   package Ok is new Untagged (Integer);

   type Void is tagged null record;

   package Err is new Untagged (Void);

begin
   null;
end B001_Tagged;
