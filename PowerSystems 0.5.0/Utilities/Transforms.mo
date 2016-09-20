within PowerSystems.Utilities;
package Transforms "Transform functions"
  extends Modelica.Icons.Package;

  constant Real[3, 3] Park0=[[2, -1, -1]/sqrt(6); [0, 1, -1]/sqrt(2); [1, 1, 1]/sqrt(3)]
    "Orthogonal transform = Park(theta=0)";
  constant Real[3, 3] J_abc=[0,-1,1; 1,0,-1; -1,1,0]/sqrt(3)
    "Rotation (pi/2) around {1,1,1} and projection on orth plane";
  //constant Real[3, 3] J_abc=skew(fill(sqrt(1/3), 3))/ "alternative";
  //J_abc = P0'*J_dq0*P0 = Park'*J_dq0*Park
  constant Real[3, 3] J_dq0=[0,-1,0; 1,0,0; 0,0,0]
    "Rotation (pi/2) around {0,0,1} and projection on orth plane";
  //J_dq0 = P0*J_abc*P0' = Park*J_abc*Park'

  function j_abc
    "Rotation(pi/2) of vector around {1,1,1} and projection on orth plane"
      extends Modelica.Icons.Function;

      input Real[3] u "vector (voltage, current)";
      output Real[3] y "rotated vector (voltage, current)";
  protected
      constant Real s13=sqrt(1/3);

  algorithm
      y := s13*{u[3]-u[2], u[1]-u[3], u[2]-u[1]};
      annotation (Inline=true, smoothOrder=2,
    Documentation(info="<html>
<p>The function <tt>j_abc(u)</tt> is a rotation of u by +90 degrees around the axis {1,1,1}.</p>
<p>The notation is chosen in analogy to the expression
<pre>  j*omega*u</pre>
used in complex plane with
<pre>
  j: imaginary unit
 (omega: angular frequency)
  u: complex vector (voltage or current).
</pre></p>
<p>The matrix expression corresponding to
<pre>  j*u</pre>
is
<pre>  J_abc*u = ([0,-1,1; 1,0,-1; -1,1,0]/sqrt(3))*u</pre></p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>"));
  end j_abc;

    function jj_abc
    "Rotation(pi/2) of vector around {1,1,1} and projection on orth plane"
      extends Modelica.Icons.Function;

      input Real[3,:] u "array of 3-vectors (voltage, current)";
      output Real[3,size(u,2)] y "array of rotated vectors (voltage, current)";
  protected
      constant Real s13=sqrt(1/3);

    algorithm
      y := s13*{u[3,:]-u[2,:], u[1,:]-u[3,:], u[2,:]-u[1,:]};
      annotation (Inline=true, smoothOrder=2,
    Documentation(info="<html>
<p>The function <tt>jj_abc(u)</tt> corresponds to <a href=\"modelica://PowerSystems.Utilities.Transforms.j_abc\">j_abc</a> but has a matrix argument u.<br>
It acts on the first index in the same way as j_abc for all values of the second index.
</html>"));
    end jj_abc;

    function j_dq0
    "Rotation(pi/2) of vector around {0,0,1} and projection on orth plane"
      extends Modelica.Icons.Function;

      input Real[:] u "vector (voltage, current)";
      output Real[size(u,1)] y "rotated vector (voltage, current)";

    algorithm
      y := cat(1, {-u[2], u[1]}, zeros(size(u,1)-2));
      annotation (Inline=true, smoothOrder=2,
    Documentation(info="<html>
<p>The function <tt>j_dq0(u)</tt> is a projection of u onto the dq-plane and a rotation by +90 degrees around the axis {0,0,1}.</p>
<p>The notation is chosen in analogy to the expression
<pre>  j*omega*u</pre>
used in complex 2-dimensional notation with
<pre>
  j: imaginary unit
 (omega: angular frequency)
  u: complex vector (voltage or current).
</pre></p>
<p>The matrix expression corresponding to
<pre>  j*u</pre>
is
<pre>  J_dq0*u = [0,-1,0; 1,0,0; 0,0,0]*u</pre></p>
<p>Note: If the argument u is 2-dimensional, then <tt>j_dq0(u)</tt> is the restriction of <tt>j_dq0(u)</tt> to the dq-plane.</p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>"));
    end j_dq0;

    function jj_dq0
    "Rotation(pi/2) of vector around {0,0,1} and projection on orth plane"
      extends Modelica.Icons.Function;

      input Real[:,:] u "array of 3- (or 2-) vectors (voltage, current)";
      output Real[size(u,1),size(u,2)] y
      "array of rotated vectors (voltage, current)";

    algorithm
      y := cat(1, {-u[2,:], u[1,:]}, zeros(size(u,1)-2, size(u,2))); // Dymola implementation now ok?
    //  y := cat(1, {-u[2,1:size(u,2)], u[1,1:size(u,2)]}, zeros(size(u,1)-2, size(u,2))); // preliminary until bug removed
      annotation (Inline=true, smoothOrder=2,
    Documentation(info="<html>
<p>The function <tt>jj_dq0(u)</tt> corresponds to <a href=\"modelica://PowerSystems.Utilities.Transforms.j_dq0\">j_dq0</a> but has a matrix argument u.<br>
It acts on the first index in the same way as j_dq0 for all values of the second index.
</html>"));
    end jj_dq0;

    function park "Park transform"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta "transformation angle";
      output Real[3,3] P "Park transformation matrix";
  protected
      constant Real s13=sqrt(1/3);
      constant Real s23=sqrt(2/3);
      constant Real dph_b=2*Modelica.Constants.pi/3;
      constant Real dph_c=4*Modelica.Constants.pi/3;
      Real[3] c;
      Real[3] s;

    algorithm
      c := cos({theta, theta - dph_b, theta - dph_c});
      s := sin({theta, theta - dph_b, theta - dph_c});
      P := transpose([s23*c, -s23*s, {s13, s13, s13}]);
      annotation (Inline=true, derivative = PowerSystems.Utilities.Transforms.der_park,
    Documentation(info="<html>
<p>The function <tt>park</tt> calculates the matrix <tt>P</tt> that transforms abc variables into dq0 variables with arbitrary angular orientation <tt>theta</tt>.<br>
<tt>P</tt> can be factorised into a constant, angle independent orthogonal matrix <tt>P0</tt> and an angle-dependent rotation <tt>R</tt></p>
<pre>
  P(theta) = R'(theta)*P0
</pre>
<p>Using the definition</p>
<pre>
  c_k = cos(theta - k*2*pi/3),  k=0,1,2 (phases a, b, c)
  s_k = sin(theta - k*2*pi/3),  k=0,1,2 (phases a, b, c)
</pre>
<p>it takes the form
<pre>
                       [ c_0,  c_1, c_2]
  P(theta) = sqrt(2/3)*[-s_0, -s_1,-s_2]
                       [ w,    w,   w  ]
</pre>
with
<pre>
                        [ 1,      -1/2,       -1/2]
  P0 = P(0) = sqrt(2/3)*[ 0, sqrt(3)/2, -sqrt(3)/2]
                        [ w,         w,          w]
</pre>
and
<pre>
             [c_0, -s_0, 0]
  R(theta) = [s_0,  c_0, 0]
             [  0,  0,   1]
</pre></p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>"));
    end park;

    function der_park "Derivative of Park transform"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta "transformation angle";
      input Modelica.SIunits.AngularFrequency omega "d/dt theta";
      output Real[3, 3] der_P "d/dt park";
  protected
      constant Real s23=sqrt(2/3);
      constant Real dph_b=2*Modelica.Constants.pi/3;
      constant Real dph_c=4*Modelica.Constants.pi/3;
      Real[3] c;
      Real[3] s;
      Real s23omega;

    algorithm
      s23omega := s23*omega;
      c := cos({theta, theta - dph_b, theta - dph_c});
      s := sin({theta, theta - dph_b, theta - dph_c});
      der_P := transpose([-s23omega*s, -s23omega*c, {0, 0, 0}]);
    annotation(Inline=true, derivative(order=2) = PowerSystems.Utilities.Transforms.der2_park,
    Documentation(info="<html>
<p>First derivative of function park(theta) with respect to time.</p>
</html>"));
    end der_park;

    function der2_park "2nd derivative of Park transform"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta "transformation angle";
      input Modelica.SIunits.AngularFrequency omega "d/dt theta";
      input Modelica.SIunits.AngularAcceleration omega_dot "d/dt omega";
      output Real[3, 3] der2_P "d2/dt2 park";
  protected
      constant Real s23=sqrt(2/3);
      constant Real dph_b=2*Modelica.Constants.pi/3;
      constant Real dph_c=4*Modelica.Constants.pi/3;
      Real[3] c;
      Real[3] s;
      Real s23omega_dot;
      Real s23omega2;

    algorithm
      s23omega_dot := s23*omega_dot;
      s23omega2 := s23*omega*omega;
      c := cos({theta, theta - dph_b, theta - dph_c});
      s := sin({theta, theta - dph_b, theta - dph_c});
      der2_P := transpose([-s23omega_dot*s - s23omega2*c, -s23omega_dot*c + s23omega2*s, {0, 0, 0}]);
    annotation(Inline=true, Documentation(info="<html>
<p>Second derivative of function park(theta) with respect to time.</p>
</html>"));
    end der2_park;

    function rotation_dq "Rotation matrix dq"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta "rotation angle";
      output Real[2, 2] R_dq "rotation matrix";
  protected
      Real c;
      Real s;

    algorithm
      c :=  cos(theta);
      s :=  sin(theta);
      R_dq :=  [c, -s; s, c];
      annotation (Inline=true, derivative = PowerSystems.Utilities.Transforms.der_rotation_dq,
    Documentation(info="<html>
<p>The function <tt>rotation_dq</tt> calculates the matrix <tt>R_dq</tt> that is the restriction of <tt>R_dq0</tt> from dq0 to dq.</p>
<p>The matrix <tt>R_dq0</tt> rotates dq0 variables around the o-axis in dq0-space with arbitrary angle <tt>theta</tt>.
<p>It takes the form
<pre>
                 [cos(theta), -sin(theta), 0]
  R_dq0(theta) = [sin(theta),  cos(theta), 0]
                 [  0,           0,        1]
</pre>
and has the real eigenvector
<pre>  {0, 0, 1}</pre>
in the dq0 reference-frame.</p>
<p>Coefficient matrices of the form (symmetrical systems)
<pre>
      [x, 0, 0 ]
  X = [0, x, 0 ]
      [0, 0, xo]
</pre>
are invariant under transformations R_dq0</p>
<p>The connection between R_dq0 and R_abc is the following
<pre>  R_dq0 = P0*R_abc*P0'.</pre>
with P0 the orthogonal transform 'Transforms.P0'.</p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>
"));
    end rotation_dq;

    function der_rotation_dq "Derivative of rotation matrix dq"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta;
      input Modelica.SIunits.AngularFrequency omega "d/dt theta";
      output Real[2, 2] der_R_dq "d/dt rotation_dq";
  protected
      Real dc;
      Real ds;

    algorithm
      dc :=  -omega*sin(theta);
      ds :=   omega*cos(theta);
      der_R_dq :=  [dc, -ds; ds, dc];
    annotation(Inline=true, derivative(order=2) = PowerSystems.Utilities.Transforms.der2_rotation_dq,
    Documentation(info="<html>
<p>First derivative of function rotation_dq(theta) with respect to time.</p>
</html>"));
    end der_rotation_dq;

    function der2_rotation_dq "2nd derivative of rotation matrix dq"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta;
      input Modelica.SIunits.AngularFrequency omega "d/dt theta";
      input Modelica.SIunits.AngularAcceleration omega_dot "d/dt omega";
      output Real[2, 2] der2_R_dq "d/2dt2 rotation_dq";
  protected
      Real c;
      Real s;
      Real d2c;
      Real d2s;
      Real omega2=omega*omega;

    algorithm
      c := cos(theta);
      s := sin(theta);
      d2c := -omega_dot*s - omega2*c;
      d2s :=  omega_dot*c - omega2*s;
      der2_R_dq := [d2c, -d2s; d2s, d2c];
    annotation(Inline=true, Documentation(info="<html>
<p>Second derivative of function rotation_dq(theta) with respect to time.</p>
</html>"));
    end der2_rotation_dq;

    function rotation_abc "Rotation matrix abc"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta "rotation angle";
      output Real[3,3] R_abc "rotation matrix";
  protected
      constant Real q13=1/3;
      constant Real s13=1/sqrt(3);
      Real c;
      Real ac;
      Real bs;
      Real[3] g;

    algorithm
      c := cos(theta);
      ac := q13*(1 - c);
      bs := s13*sin(theta);

      g := {ac + c, ac + bs, ac - bs};
      R_abc := [g[{1,2,3}], g[{3,1,2}], g[{2,3,1}]];
      annotation (Inline=true, derivative = PowerSystems.Utilities.Transforms.der_rotation_abc,
    Documentation(info="<html>
<p>The function <tt>rotation_abc</tt> calculates the matrix <tt>R_abc</tt> that rotates abc variables around the {1,1,1}-axis in abc-space with arbitrary angle <tt>theta</tt>.
<p>Using the definition
<pre>
  g_k = 1/3 + (2/3)*cos(theta - k*2*pi/3),  k=0,1,2 (phases a, b, c)
</pre>
it takes the form
<pre>
                 [g_0, g_2, g_1]
  R_abc(theta) = [g_1, g_0, g_2]
                 [g_2, g_1, g_0]
</pre>
and has the real eigenvector
<pre>  {1, 1, 1}/sqrt(3)</pre>
in the abc reference-frame.</p>
<p>Coefficient matrices of the form (symmetrical systems)
<pre>
      [x,  xm, xm]
  X = [xm,  x, xm]
      [xm, xm, x ]
</pre>
are invariant under transformations R_abc</p>
<p>The connection between R_abc and R_dq0 is the following
<pre>  R_abc = P0'*R_dq0*P0.</pre>
with P0 the orthogonal transform 'Transforms.P0'.</p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>"));
    end rotation_abc;

    function der_rotation_abc "Derivative of rotation matrix abc"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta;
      input Modelica.SIunits.AngularFrequency omega "d/dt theta";
      output Real[3, 3] der_R_abc "d/dt rotation_abc";
  protected
      constant Real q13=1/3;
      constant Real s13=1/sqrt(3);
      Real s;
      Real as;
      Real bc;
      Real[3] dg;

    algorithm
      s := sin(theta);
      as := q13*s;
      bc := s13*cos(theta);

      dg := omega*{as - s, as + bc, as - bc};
      der_R_abc := [dg[{1,2,3}], dg[{3,1,2}], dg[{2,3,1}]];
    annotation(Inline=true, derivative(order=2) = PowerSystems.Utilities.Transforms.der2_rotation_abc,
    Documentation(info="<html>
<p>First derivative of function rotation_abc(theta) with respect to time.</p>
</html>"));
    end der_rotation_abc;

    function der2_rotation_abc "2nd derivative of rotation matrix abc"
      extends Modelica.Icons.Function;

      input Modelica.SIunits.Angle theta;
      input Modelica.SIunits.AngularFrequency omega "d/dt theta";
      input Modelica.SIunits.AngularAcceleration omega_dot "d/dt omega";
      output Real[3, 3] der2_R_abc "d2/dt2 rotation_abc";
  protected
      constant Real q13=1/3;
      constant Real s13=1/sqrt(3);
      Real c;
      Real s;
      Real ac;
      Real as;
      Real bc;
      Real bs;
      Real[3] d2g;

    algorithm
      c := cos(theta);
      s := sin(theta);
      ac := q13*c;
      as := q13*s;
      bc := s13*cos(theta);
      bs := s13*sin(theta);

      d2g := omega*omega*{ac - c, ac - bs, ac + bs} + omega_dot*{as - s, as + bc, as - bc};
      der2_R_abc := [d2g[{1,2,3}], d2g[{3,1,2}], d2g[{2,3,1}]];
    annotation(Inline=true, Documentation(info="<html>
<p>Second derivative of function rotation_abc(theta) with respect to time.</p>
</html>"));
    end der2_rotation_abc;

    function permutation "Permutation of vector components"
      extends Modelica.Icons.Function;

      input Integer s(min=-1,max=1) "(-1, 0, 1), numbers permutation";
      input Real[3] u "vector";
      output Real[3] v "permuted vector";

    algorithm
      if s == 1 then
        v := u[{2,3,1}];
      elseif s == -1 then
        v := u[{3,1,2}];
      else
        v := u;
      end if;
      annotation (Inline=true, smoothOrder=2,
    Documentation(info="<html>
<p>Positive permutation of 3-vector.</p>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>"));
    end permutation;

    function der_permutation "Derivative of permutation of vector components"
      extends Modelica.Icons.Function;

      input Integer s(min=-1,max=1) "(-1, 0, 1), numbers permutation";
      input Real[3] u "vector";
      input Real[3] der_u "d/dt u";
      output Real[3] der_v "d/dt permutation";

    algorithm
      if s == 1 then
        der_v := der_u[{2,3,1}];
      elseif s == -1 then
        der_v := der_u[{3,1,2}];
      else
        der_v := der_u;
      end if;
    annotation(Inline=true, derivative(order2) = PowerSystems.Utilities.Transforms.der2_permutation,
    Documentation(info="<html>
<p>First derivative of Transforms.permutation with respect to time.</p>
</html>"));
    end der_permutation;

    function der2_permutation
    "2nd derivative of permutation of vector components"
      extends Modelica.Icons.Function;

      input Integer s(min=-1,max=1) "(-1, 0, 1), numbers permutation";
      input Real[3] u "vector";
      input Real[3] der_u "d/dt u";
      input Real[3] der2_u "d2/dt2 u";
      output Real[3] der2_v "d2/dt2 permutation";

    algorithm
      if s == 1 then
        der2_v := der2_u[{2,3,1}];
      elseif s == -1 then
        der2_v := der2_u[{3,1,2}];
      else
        der2_v := der2_u;
      end if;
    annotation(Inline=true, Documentation(info="<html>
<p>Second derivative of Transforms.permutation with respect to time.</p>
</html>"));
    end der2_permutation;

    annotation (preferredView="info",
  Documentation(info="<html>
<p><a href=\"modelica://PowerSystems.UsersGuide.Introduction.Transforms\">up users guide</a></p>
</html>
"));
end Transforms;

