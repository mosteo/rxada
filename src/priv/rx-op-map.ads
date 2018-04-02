with Rx.Impl.Transformers;

generic
   with package Typed is new Rx.Impl.Transformers (<>);
package Rx.Op.Map with Preelaborate is

   function Create (F : Typed.Actions.Func1) return Typed.Operator'Class;

   function "&" (Producer : Typed.From.Observable;
                 Consumer : Typed.Actions.Func1)
                 return Typed.Into.Observable;
   --  Since Ada has this unlike Java, we can have a special case concatenator for the Map operation,
   --  that saves explicit "Map":
   --     Some_Operator & Some_Action'Access & Some_Other_Operator
   --  instead of:
   --     Some_Operator & Map (Some_Action'Access) & Some_Other_Operator

end Rx.Op.Map;
