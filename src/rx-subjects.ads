with Rx.Errors;
with Rx.Impl.Transformers;

generic
   with package Transformer is new Rx.Impl.Transformers (<>);
package Rx.Subjects with Preelaborate is

   --  Subjects are both observable and observer, with state

   type Subject Is
     new Transformer.From.Contracts.Observer
     and Transformer.Into.Contracts.Observable with private;

   overriding procedure On_Next      (This : in out Subject; V : Transformer.From.T) is null;
   overriding procedure On_Complete  (This : in out Subject) is null;
   overriding procedure On_Error     (This : in out Subject; Error : Errors.Occurrence) is null;

   overriding procedure Subscribe (Producer : in out Subject;
                                   Consumer : in out Transformer.Into.Observer'Class) is null;

private

   type Subject Is
     new Transformer.From.Contracts.Observer
     and Transformer.Into.Contracts.Observable with null record;

end Rx.Subjects;
