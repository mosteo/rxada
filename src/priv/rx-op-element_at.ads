with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Element_At with Preelaborate is

   function Create (Pos   : Rx_Integer;
                    First : Rx_Integer := 1) -- 1-based by default
                    return Operate.Operator'Class
     with Pre => Pos >= First;
   --  If Pos is not reached, Constraint_Error

   function Or_Default (Default : Operate.T;
                        Pos     : Rx_Integer;
                        First   : Rx_Integer := 1)
                        return Operate.Operator'Class
     with Pre => Pos >= First;

end Rx.Op.Element_At;
