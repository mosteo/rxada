with Rx.Tools.Holders;

package body Rx.Src.Defer is

   package Holders is new Rx.Tools.Holders (Factories.Observable_Factory'Class,
                                            "defer.factory'class");

   type Some_Factory is new Holders.Definite with null record;

   type Observable is new Typed.Contracts.Observable with record
      Factory : Some_Factory;
   end record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer : in out Typed.Observer'Class)
   is
      Actual : Typed.Observable'Class := Producer.Factory.CRef.Subscribe;
   begin
      Actual.Subscribe (Consumer);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (F : Factories.Observable_Factory'Class) return Typed.Observable is
   begin
      return Observable'(Typed.Contracts.Observable with Factory => Hold (F));
   end Create;

   type Func_Factory is new Factories.Observable_Factory with record
      Func : Factories.Observable_Factory_Func;
   end record;

   overriding function Subscribe (F : Func_Factory) return Typed.Observable is (F.Func.all);

   function Create (F : Factories.Observable_Factory_Func) return Typed.Observable is
   begin
      return Observable'(Typed.Contracts.Observable with Factory => Hold (Func_Factory'(Func => F)));
   end Create;

end Rx.Src.Defer;
