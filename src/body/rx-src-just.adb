with Rx.Src.Create;

package body Rx.Src.Just is

   procedure On_Subscribe (State    :        Typed.D;
                           Observer : in out Typed.Observer)
   is
   begin
      Observer.On_Next (Typed.Type_Traits.To_Indefinite (State));
   end On_Subscribe;

   package SrcCreate is new Src.Create (Typed);
   package Source is new SrcCreate.With_State (Typed.D, On_Subscribe);

   function Create (V : Typed.T) return Typed.Observable'Class is
   begin
      return Source.Create (Typed.Type_Traits.To_Definite (V));
   end Create;

end Rx.Src.Just;
