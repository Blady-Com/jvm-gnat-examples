
package body Complex is

   function Real (Z : Complex_Number) return Float is
   begin
      return Z.Real;
   end Real;

   function Imaginary (Z : Complex_Number) return Float is
   begin
      return Z.Imaginary;
   end Imaginary;

   procedure Build (X, Y : Float;
                    Z : out Complex_Number) is
   begin
      Z.Real := X;
      Z.Imaginary := Y;
   end Build;

   procedure Square
     (Z : Complex_Number;
      Result : out Complex_Number)
   is
      Tmp : Float;
   begin
      Tmp := Z.Real * Z.Real - Z.Imaginary * Z.Imaginary;
      Result.Imaginary := 2.0 * Z.Real * Z.Imaginary;
      Result.Real := Tmp;
   end Square;

   procedure Add (Z1 : Complex_Number;
                  Z2 : Complex_Number;
                  Result : out Complex_Number) is
   begin
      Result.Real      := Z1.Real + Z2.Real;
      Result.Imaginary := Z1.Imaginary + Z2.Imaginary;
   end Add;


   function Norm (Z : Complex_Number) return Float is
   begin
      return Z.Real * Z.Real + Z.Imaginary * Z.Imaginary;
   end Norm;

end Complex;

