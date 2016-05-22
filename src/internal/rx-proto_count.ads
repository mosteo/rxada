with Rx.Operator;
with Rx.Producer;

generic
   type T (<>) is private;
package Rx.Proto_Count is

   package Bind is new Rx.Producer (T);

   package Operator is new Rx.Operator (Bind);

end Rx.Proto_Count;
