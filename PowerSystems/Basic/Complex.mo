within PowerSystems.Basic;
package Complex "Complex functions (preliminary package)"
  extends Modelica.Icons.Package;

  type ComplexType = Real[2, 2] "matrix representation of complex number";

  constant ComplexType re = [1,0;0,1] "matrix representation of real unit";
  constant ComplexType im = [0,-1;1,0]
    "matrix representation of imaginary unit";

  function conjC "Conjugate value of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType conj_z "conjugate value of z";

  algorithm
    conj_z := transpose(z);
  annotation (Documentation(info="<html>
</html>"));
  end conjC;

  function absC "Absolute value of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output Real abs_z "absolute value of z";

  algorithm
    abs_z := sqrt(detC(z));
  annotation (Documentation(info="<html>
</html>"));
  end absC;

  function detC "Determinant of complex number matrix"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output Real det_z "determinant of z";

  algorithm
    det_z := z[1,1]*z[2,2] - z[1,2]*z[2,1];
  annotation (Documentation(info="<html>
</html>"));
  end detC;

  function invC "Inverse of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType inv_z "inverse of z";

  algorithm
    inv_z := transpose(z)/detC(z);
  annotation (Documentation(info="<html>
</html>"));
  end invC;

  function sumC "Sum of complex numbers"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType[          :] z "complex vector";
    output ComplexType s "sum of components of z";

  algorithm
    s := z[1, :, :];
    for k in 2:size(z,1) loop
      s := s + z[k,:,:];
    end for;
  annotation (Documentation(info="<html>
</html>"));
  end sumC;

  function prodC "Product of complex numbers"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType[          :] z "complex vector";
    output ComplexType p "product of components of z";

  algorithm
    p := z[1, :, :];
    for k in 2:size(z,1) loop
      p := p*z[k,:,:];
    end for;
  annotation (Documentation(info="<html>
</html>"));
  end prodC;

  function expI "Exponential of imaginary number"
    extends PowerSystems.Basic.Icons.Function;

    input Real phi "real argument";
    output ComplexType exp_jphi "exponential of j*phi";
  protected
    Real c;
    Real s;

  algorithm
    c := cos(phi);
    s := sin(phi);
    exp_jphi := [c, -s; s, c];
  annotation (Documentation(info="<html>
</html>"));
  end expI;

  function expC "Exponential of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType exp_z "exponential of z";

  algorithm
    exp_z := exp(z[1,1])*expI(z[2,1]);
  annotation (Documentation(info="<html>
</html>"));
  end expC;

  function powerC "Power of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    input Real alpha "exponent";
    output ComplexType pow_z "power of z";
  protected
    function atan2=Modelica.Math.atan2;

  algorithm
    pow_z := detC(z)^(0.5*alpha)*expI(alpha*atan2(z[2,1], z[1,1]));
  annotation (Documentation(info="<html>
</html>"));
  end powerC;

  function sqrtC "Square-root of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType sqrt_z "square-root of z";
  protected
    Real a;
    function atan2=Modelica.Math.atan2;

  algorithm
    a := absC(z);
    sqrt_z := sqrt(a)*expI(0.5*atan2(z[2,1], z[1,1]));
  annotation (Documentation(info="<html>
</html>"));
  end sqrtC;

  function logC "Logarithm of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType log_z "logarithm of z";
  protected
    Real re;
    Real im;
    function atan2=Modelica.Math.atan2;
    function log=Modelica.Math.log;

  algorithm
    re := 0.5*log(detC(z));
    im := atan2(z[2,1], z[1,1]);
    log_z := [re, -im; im, re];
  annotation (Documentation(info="<html>
</html>"));
  end logC;

  function cosC "Cosine of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType cos_z "cosine of z";
  protected
    Real re;
    Real im;

  algorithm
    re := cos(z[1,1])*cosh(z[2,1]);
    im := -sin(z[1,1])*sinh(z[2,1]);
    cos_z := [re, -im; im, re];
  annotation (Documentation(info="<html>
</html>"));
  end cosC;

  function sinC "Sine of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType sin_z "sine of z";
  protected
    Real re;
    Real im;

  algorithm
    re := sin(z[1,1])*cosh(z[2,1]);
    im := cos(z[1,1])*sinh(z[2,1]);
    sin_z := [re, -im; im, re];
  annotation (Documentation(info="<html>
</html>"));
  end sinC;

  function tanC "Tangens of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType tan_z "tangens of z";

  algorithm
    tan_z := sinC(z)*invC(cosC(z));
  annotation (Documentation(info="<html>
</html>"));
  end tanC;

  function atanC "Arc-tangens of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType atan_z "arc-tangens of z";
  protected
    Real a;

  algorithm
    a := 0.5*(1 - detC(z));
    atan_z := 0.5*logC([a, -z[1,1]; z[1,1], a]/(1 - a + z[2,1]));
    atan_z := [atan_z[2,1], atan_z[2,2]; -atan_z[1,1], -atan_z[1,2]];
  annotation (Documentation(info="<html>
</html>"));
  end atanC;

  function coshC "Hyperbolic cosine of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType cosh_z "hyp cosine of z";
  protected
    Real re;
    Real im;

  algorithm
    re := cosh(z[1,1])*cos(z[2,1]);
    im := sinh(z[1,1])*sin(z[2,1]);
    cosh_z := [re, -im; im, re];
  annotation (Documentation(info="<html>
</html>"));
  end coshC;

  function sinhC "Hyperbolic sine of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType sinh_z "hyp sine of z";
  protected
    Real re;
    Real im;

  algorithm
    re := sinh(z[1,1])*cos(z[2,1]);
    im := cosh(z[1,1])*sin(z[2,1]);
    sinh_z := [re, -im; im, re];
  annotation (Documentation(info="<html>
</html>"));
  end sinhC;

  function tanhC "Hyperbolic tangens of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType tanh_z "hyp tangens of z";

  algorithm
    tanh_z := sinhC(z)*invC(coshC(z));
  annotation (Documentation(info="<html>
</html>"));
  end tanhC;

  function atanhC "Area-tangens-hyp of complex number"
    extends PowerSystems.Basic.Icons.Function;

    input ComplexType z "complex argument";
    output ComplexType atan_z "area-tangens-hyp of z";
  protected
    Real a;

  algorithm
    a := 0.5*(1 - detC(z));
    atan_z := 0.5*logC([a, -z[2,1]; z[2,1], a]/(1 - a - z[1,1]));
  annotation (Documentation(info="<html>
</html>"));
  end atanhC;
    annotation (preferredView="info",
  Documentation(info="<html>
<p>Auxiliary package, to be used until Complex type is available in Modelica.
Complex numbers are represented as 2x2 matrix, such that arithmetic operations on complex numbers become equivalent to arithmetic operations on their matrix representations.</p>
<p>Complex number <tt>(x + jy)</tt>, considered as an element of an algebra, and represented by the real 2x2 matrix z:</p>
<pre>
  z = [x, -y]
      [y,  x]
</pre>
<p>The following relations hold for</p>
<pre>
- complex conjugate:   transpose(z)
- addition:            z1 + z2
- multiplication:      z1*z2
- inverse:             transpose(z)/det(z)
- absolute value:      sqrt(det(z))
</pre>
<p>where <tt>det(z)</tt> denotes the determinant of matrix z.</p>
<p><b>Example:</b></p>
<p>The linear differential equation with constant coefficients for a real scalar current I and voltage V
<pre>  L*dI/dt + R*I = V</pre>
is mapped by Fourier-transform to the algebraic equation
<pre>  (R + j*omega*L)*i = v</pre>
with complex i and v, if for simplicity only one frequency term is assumed. Using
<pre>
  Complex Z =  R*re_unit + omega*L*im_unit (complex impedance)
  Complex i = i1*re_unit + i2*im_unit      ('current phasor')
  Complex v = v1*re_unit + v2*im_unit      ('voltage phasor')
</pre>
the equation finally becomes
<pre>  Z*i = v</pre></p>
</html>
"));
end Complex;
