generic

   type Task_Type (<>) is limited private;
   type Task_Ptr is access Task_Type;

procedure Rx.Impl.Task_Deallocation (T : Task_Ptr) with Preelaborate;

--  See http://www.adacore.com/developers/development-log/NF-65-H911-007-gnat and
--  https://groups.google.com/d/msg/comp.lang.ada/6p-_Dwjlr4o/POiIWk6AX0cJ

