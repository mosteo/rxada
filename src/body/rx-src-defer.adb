with Rx.Holders;

package body Rx.Src.Defer is

   package Factories is new Rx.Holders (Factory'Class);

   type Some_Factory is new Factories.Definite with null record;

   type Observable is new Typed.Contracts.Observable with record
      Factory : Some_Factory;
   end record;

   overriding
   procedure Subscribe (Producer : in out Observable;
                        Consumer :        Typed.Subscriber'Class)
   is
      Actual : Typed.Observable'Class := Producer.Factory.CRef.On_Subscribe;
   begin
      Actual.Subscribe (Consumer);
   end Subscribe;

   ------------
   -- Create --
   ------------

   function Create (F : Factory'Class) return Typed.Observable is
   begin
      return Observable'(Typed.Contracts.Observable with Factory => Hold (F));
   end Create;

   type Func_Factory is new Factory with record
      Func : Factory_Func;
   end record;

   overriding function On_Subscribe (F : Func_Factory) return Typed.Observable is (F.Func.all);

   function Create (F : Factory_Func) return Typed.Observable is
   begin
      return Observable'(Typed.Contracts.Observable with Factory => Hold (Func_Factory'(Func => F)));
   end Create;

end Rx.Src.Defer;
