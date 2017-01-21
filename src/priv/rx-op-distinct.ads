with Rx.Impl.Preservers;

generic
   with package Operate is new Rx.Impl.Preservers (<>);
package Rx.Op.Distinct is

   function Default_Not_Same (L, R : Operate.T) return Boolean;

   function Create (Are_Distinct : Operate.Typed.Actions.Comparator := Default_Not_Same'Access)
                    return Operate.Operator'Class;
   --  When null, default "=" is used to compare elements

private

   use type Operate.T;

   function Default_Not_Same (L, R : Operate.T) return Boolean is (L /= R);

end Rx.Op.Distinct;
