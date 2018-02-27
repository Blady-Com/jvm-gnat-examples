
package Complex is

   type Complex_Number is private;

   function Real (Z : Complex_Number) return Float;
   function Imaginary (Z : Complex_Number) return Float;

   procedure Build (X, Y : Float;
                    Z : out Complex_Number);
   procedure Square (Z : Complex_Number;
                     Result : out Complex_Number);
   procedure Add (Z1 : Complex_Number;
                  Z2 : Complex_Number;
                  Result : out Complex_Number);
   function Norm (Z : Complex_Number)
                  return Float;

private
   type Complex_Number is record
      Real      : Float;
      Imaginary : Float;
   end record;


end Complex;
