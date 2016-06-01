with Ada.Containers.Indefinite_Holders;

with Rx.Values;

package Rx.Actions is

   pragma Preelaborate;

   type Func1 is interface;
   function Call (F : Func1; V : Values.Value'Class) return Values.Value'Class is abstract;

   type Proc1 is interface;
   procedure Call (P : Proc1; V : Values.Value'Class) is abstract;

   No_Op : constant Proc1'Class;

   --  Holders
   package H_Func1 is new Ada.Containers.Indefinite_Holders (Func1'Class);
   package H_Proc1 is new Ada.Containers.Indefinite_Holders (Proc1'Class);

private

   type Noop_Proc1 is new Proc1 with null record;

   overriding
   procedure Call (P : Noop_Proc1; V : Values.Value'Class) is null;

   No_Op : constant Proc1'Class := Noop_Proc1'(null record);

end Rx.Actions;
