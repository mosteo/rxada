--  Root package for policies/traits
package Rx.Traits is

   pragma Pure;

   type Printable is interface;
   function Image (P : Printable) return String is abstract;

end Rx.Traits;
