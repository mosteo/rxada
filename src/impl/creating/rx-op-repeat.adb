with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Rx.Op.Repeat is

   use type Operate.T;

   package T_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Operate.T);

   use type Actions.HTFilter0;

   type Kinds is (Counter, While_Do, Repeat_Until);

   type Operator (Kind : Kinds) is new Operate.Operator with record
      Sequence : T_Lists.List;

      First_Seen : Boolean := False;

      case Kind is
         when Counter => Repeats : Positive;
         when others  => Filter  : Actions.HTFilter0;
      end case;
   end record;

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T;
                      Child : in out Operate.Observer);

   overriding
   procedure On_Completed (This : in out Operator;
                           Child : in out Operate.Observer);

   overriding
   procedure On_Next (This  : in out Operator;
                      V     :        Operate.T;
                      Child : in out Operate.Observer) is
   begin
      if This.Kind = While_Do and then not This.First_Seen and then not This.Filter.Ref.Check then
         Child.On_Completed;
         This.Unsubscribe;
      else
         This.First_Seen := True;
         This.Sequence.Append (V);
         Child.On_Next (V);
      end if;
   end On_Next;

   overriding
   procedure On_Completed (This : in out Operator;
                           Child : in out Operate.Observer)
   is
      Check : Boolean;
   begin
      if not This.Is_Subscribed then -- Because its While_Do and first check failed
         return;
      end if;

      if not This.Sequence.Is_Empty then
         case This.Kind is
            when Counter =>
               for I in 1 .. This.Repeats loop
                  for V of This.Sequence loop
                     Child.On_Next (V);
                  end loop;
               end loop;

            when While_Do | Repeat_Until =>
               loop
                  Check := This.Filter.Ref.Check;
                  exit when (This.Kind = Repeat_Until and then     Check) or else
                            (This.Kind = While_Do     and then not Check);

                  for V of This.Sequence loop
                     Child.On_Next (V);
                  end loop;
               end loop;
         end case;
      end if;

      Child.On_Completed;
   end On_Completed;

   --------------------
   -- Repeat_Forever --
   --------------------

   function Repeat_Forever return Operate.Operator'Class is
   begin
      return Operator'(Operate.Operator with
                       Kind   => While_Do,
                       Filter => + Actions.Wrap (Always'Access),
                       others => <>);
   end Repeat_Forever;

   ------------
   -- Repeat --
   ------------

   function Repeat (Times : Positive) return Operate.Operator'Class is
   begin
            return Operator'(Operate.Operator with
                       Kind    => Counter,
                       Repeats => Times,
                       others  => <>);
   end Repeat;

   --------------
   -- While_Do --
   --------------

   function While_Do
     (Check : Actions.TFilter0'Class)
      return Operate.Operator'Class
   is
   begin
      return Operator'(Operate.Operator with
                       Kind   => While_Do,
                       Filter => + Check,
                       others => <>);
   end While_Do;

   ------------------
   -- Repeat_Until --
   ------------------

   function Repeat_Until
     (Check : Actions.TFilter0'Class)
      return Operate.Operator'Class
   is
   begin
      return Operator'(Operate.Operator with
                       Kind   => Repeat_Until,
                       Filter => + Check,
                       others => <>);
   end Repeat_Until;

end Rx.Op.Repeat;
