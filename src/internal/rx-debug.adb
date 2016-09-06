package body Rx.Debug is

   procedure Log (S : String; Level : Levels := Verbose) is
   begin
      if Level > Debug.Level then
         Put_Line ("debug: " & S);
      end if;
   end Log;


   procedure Put_Line (I : Integer) is
   begin
      Put_Line (I'Img);
   end Put_Line;

end Rx.Debug;
