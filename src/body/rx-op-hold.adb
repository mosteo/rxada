with Ada.Numerics.Float_Random;

with Rx.Debug;

package body Rx.Op.Hold is

   ---------------
   -- Generator --
   ---------------

   protected Generator is
      procedure Get (F : out Float);
   private
      State : Ada.Numerics.Float_Random.Generator;
   end Generator;

   protected body Generator is

      procedure Get (F : out Float) is
      begin
         F := Ada.Numerics.Float_Random.Random (State);
      end Get;

   end Generator;

   ------------
   -- Random --
   ------------

   function Random return Ada.Numerics.Float_Random.Uniformly_Distributed is
      F : Float;
   begin
      Generator.Get (F);
      return F;
   end Random;

   type Operator is new Preserver.Operator with record
      Fixed,
      Random: Float;
   end record;

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Preserver.T);

   ------------
   -- Create --
   ------------

   function Create (Fixed  : Duration;
                    Random : Duration := 0.0)
                       return Preserver.Operator'Class is
   begin
      return Operator'(Preserver.Operator with
                       Fixed  => Float (Fixed),
                       Random => Float (Random));
   end Create;

   -------------
   -- On_Next --
   -------------

   overriding procedure On_Next (This  : in out Operator;
                                 V     :        Preserver.T)
   is
      Pause : constant Float    := This.Fixed + This.Random * Random;
      Dur   : constant Duration := Duration (Pause);
   begin
      Debug.Trace ("on_next hold:" & Dur'Img);
      delay Dur;
      This.Get_Observer.On_Next (V);
   end On_Next;

end Rx.Op.Hold;
